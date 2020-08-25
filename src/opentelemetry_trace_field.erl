-module(opentelemetry_trace_field).

-include_lib("opentelemetry_api/include/opentelemetry.hrl").
-include_lib("opentelemetry/include/ot_span.hrl").

-define(TRACE_FIELD_TAB, otel_trace_field_tab).

-export([on_start/2, on_end/2, add/3, init/0]).

on_start(Span, _) ->
  Span.

on_end(Span = #span{trace_id = TraceId}, _) ->
  Tracer = opentelemetry:get_tracer(),
  SpanCtx = ot_span:get_ctx(Tracer, Span),
  try ets:lookup(?TRACE_FIELD_TAB, TraceId) of
    [{_, Attributes}] ->
      erlang:display({Span, SpanCtx, Attributes}),
      erlang:display(ot_span:set_attributes(Tracer, SpanCtx, Attributes))
  catch
    error:badarg ->
      erlang:display(false)
  end,
  Span.

init() ->
  case ets:info(?TRACE_FIELD_TAB, name) of
    undefined ->
      ets:new(?TRACE_FIELD_TAB, [public, named_table, set, {write_concurrency, true}]);
    _ ->
      ok
  end.

add(#span_ctx{trace_id = TraceId}, Key, Value) ->
  try ets:lookup(?TRACE_FIELD_TAB, TraceId) of
    Attributes ->
      ets:insert(?TRACE_FIELD_TAB, [{TraceId, [{"app." ++ Key, Value} | Attributes]}])
  catch
    error:badarg ->
      false
  end.

