-module(opentelemetry_trace_field_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/ot_span.hrl").

all() ->
  [adding_trace_field_works].

%% try for 1 second and also return the result of Y
-define(UNTIL_NOT_EQUAL(X, Y),
        fun Until(I) when I =:= 10 ->
              ct:fail(timeout);
            Until(I) ->
              R = Y,
              case X =/= R of
                true ->
                  R;
                false ->
                  timer:sleep(100),
                  Until(I + 1)
              end
        end(0)).

adding_trace_field_works() ->
  application:load(opentelemetry),
  application:set_env(opentelemetry,
                      processors,
                      [{opentelemetry_trace_field, #{}},
                       {ot_batch_processor, #{scheduled_delay_ms => 1}}]),
  {ok, _} = application:ensure_all_started(opentelemetry),
  opentelemetry_trace_field:init(),
  %% adds an exporter for a new table
  %% spans will be exported to a separate table for each of the test cases
  Tid = ets:new(exported_spans, [public, bag]),
  ot_batch_processor:set_exporter(ot_exporter_tab, Tid),
  Tracer = opentelemetry:get_tracer(),
  SpanCtx = ot_tracer:start_span(Tracer, "some_span", #{}),
  erlang:display({"Some", ot_span:set_attributes(Tracer, SpanCtx, [{"should", "not override"}])}),
  opentelemetry_trace_field:add(SpanCtx, "some_field", "some_value"),
  ot_tracer:end_span(Tracer),
  #span{trace_id = TraceId} = SpanCtx,
  erlang:display(ets:lookup(otel_trace_field_tab, TraceId)),
  assert_exported(Tid, SpanCtx),
  _ = application:stop(opentelemetry),
  application:unload(opentelemetry),
  false.

assert_exported(Tid, #span_ctx{trace_id = TraceId, span_id = SpanId}) ->
  ?UNTIL_NOT_EQUAL([],
                   ets:match_object(Tid,
                                    #span{trace_id = TraceId,
                                          span_id = SpanId,
                                          attributes = [{<<"app.some_field">>, <<"some_value">>}],
                                          _ = '_'})).

