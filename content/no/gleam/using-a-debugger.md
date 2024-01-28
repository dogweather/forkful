---
title:                "Å bruke en feilsøker"
date:                  2024-01-26T03:49:23.480290-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å bruke en debugger er i grunn det samme som å spille detektiv i koden din, snuse opp feil og finne ut hvorfor ting ikke kjører glatt. Programmerere gjør det fordi, la oss innse det, feil er uunngåelige, og å klemme dem effektivt betyr å få koden din oppe og kjøre raskere og mer pålitelig.

## Hvordan:
Gleam støtter seg for tiden på Erlang-økosystemet for verktøy, så du vil typisk feilsøke med verktøy som `rebar3`, `observer` og `debugger`. Slik blir du skitten på hendene med feilsøking:

```gleam
// I din rebar-konfig, sørg for at du har disse linjene for å inkludere feilsøkingsinfo:
{erl_opts, [debug_info]}.

// Kjør et Erlang-skall med appen din lastet
rebar3 shell

// Inne i skallet, kan du starte debugge
1> debugger:start().
```

Enkelt, ikke sant? `Debugger` GUI popper opp, og du kan sette brytepunkter, steg-for-steg gjennom kode, og se på variabler så mye du vil. Du vil ikke se Gleam-kode direkte, men Erlang-koden den kompilerer til, som fortsatt er ganske nyttig.

## Dypdykk
Gleam er et ungt språk, så selv om det står på skuldrene til Erlang-økosystemet, er ikke native Gleam feilsøkingsverktøy ennå i søkelyset. Det betyr at vi bruker Erlangs velprøvde verktøy, og det er ikke en dårlig ting. Erlangs debugger har eksistert siden 90-tallet, finslipt gjennom år med å utrydde pesky feil i systemer der pålitelighet er nøkkelen.

Når det gjelder alternativer, er sporing en kraftig metode i BEAM-verdenen (det er den virtuelle maskinen som kjører Erlang og Elixir-kode). Ved hjelp av `rebar3` kan du tappe inn i verktøy som `recon` for å spore funksjonskall og dykke dypt inn i ytelsesproblemer.

Overgangen mellom å skrive Gleam og feilsøke i Erlang kan føles som om du oversetter tankene dine på farten. Men fordelen er at du får en titt inn i Erlang-verdenen, og forstår byggesteinene i appen din i dens kjøretidsform.

## Se Også
For å utvide feilsøkingsverktøykassen din, sjekk ut:

- Erlangs debugger-dokumentasjon: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- `Recon`-biblioteket for Erlang: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
