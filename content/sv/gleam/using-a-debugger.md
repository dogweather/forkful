---
title:                "Att använda en debugger"
date:                  2024-01-26T03:49:10.005665-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda en debugger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att använda en debugger är i princip som att du spelar detektiv i din kod, snokar efter buggar och försöker förstå varför saker inte fungerar smidigt. Programmerare gör det eftersom, låt oss inse det, buggar är oundvikliga, och att krossa dem effektivt innebär att få din kod att fungera snabbare och mer tillförlitligt.

## Hur man gör:
Gleam lutar sig för närvarande på Erlang-ekosystemet för verktyg, så du kommer typiskt att felsöka med verktyg som `rebar3`, `observer` och `debugger`. Så här går du tillväga för att dyka djupt in i felsökningen:

```gleam
// I din rebar-konfig, se till att du har dessa rader för att inkludera felsökningsinfo:
{erl_opts, [debug_info]}.

// Kör en Erlang-shell med din app laddad
rebar3 shell

// Inuti shellen kan du starta debuggern
1> debugger:start().
```

Enkelt, eller hur? `debugger`-GUI:t dyker upp, och du kan sätta brytpunkter, stega igenom kod och titta på variabler så mycket du vill. Du kommer inte att se Gleam-kod direkt, men den Erlang-kod som den kompileras till, vilket fortfarande är ganska hjälpsamt.

## Djupdykning
Gleam är ett ungt språk, så även om det står på Erlang-ekosystemets axlar, är inte de infödda Gleam-felsökningsverktygen ännu i rampljuset. Det betyder att vi använder Erlangs beprövade verktyg, och det är inte en dålig sak. Erlangs debugger har funnits sedan 90-talet, finslipat genom år av att utrota störande buggar i system där tillförlitlighet är nyckeln.

När det gäller alternativ är spårning en kraftfull metod i BEAM-världen (det är den virtuella maskin som kör Erlang- och Elixir-kod). Med `rebar3` kan du tappa in i verktyg som `recon` för att spåra funktionsanrop och dyka djupt in i prestandaproblem.

Bytet mellan att skriva Gleam och felsöka i Erlang kan kännas som att du översätter dina tankar i farten. Men fördelen är att du får en titt in i Erlang-världen, förstår byggstenarna i din app i dess körningsform.

## Se även
För att utöka din felsökningsverktygslåda, kolla in:

- Erlangs debuggerdokumentation: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- `Recon`-biblioteket för Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- Om spårning i BEAM: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)