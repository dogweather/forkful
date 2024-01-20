---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall er prosessen med å produsere tall uten et gjenkjennelig mønster eller rekkefølge. Programmerere gjør det for å opprette simuleringer, kryptografiske nøkler, generere unike IDer, og mange andre oppgaver som krever uforutsigbarhet.

## Hvordan gjøre det:
Her er et eksempel på hvordan du kan generere et tilfeldig tall i Elixir:

```elixir
:rand.uniform(100)
```
Den ovennevnte koden returnerer et tilfeldig tall mellom 1 og 100. Her er et annet eksempel:

```elixir
Enum.random(1..10)
```
Dette vil generere et tilfeldig tall mellom 1 og 10. 

## Dypdykk
Historisk sett har tilfeldige tall vært vanskelige å generere fordi datamaskiner er designet for å utføre presise og deterministiske operasjoner. I Elixir bruker vi Erlang's `:rand.uniform/1` funksjonen til å generere tilfeldige tall.

Alternativer til Elixirs innebygde metoder inkluderer bruk av tredjeparts biblioteker som `exs1024`, som tilbyr et høyere nivå av tilfeldighet.

Når det gjelder gjennomføring, bruker `:rand.uniform/1` en sofistikert algoritme kjent som et Mersenne Twister for å generere sine tall. Denne algoritmen er designet for å produsere tall som er jevnt fordelt over sitt område og har en veldig lang periode før det gjentar seg.

## Se også
Du kan finne mer informasjon om generering av tilfeldige tall og Elixir programmering på følgende lenker:

- Elixir's Offisielle Dokumentasjon: https://elixirs-lang.org/docs.html
- Erlang's :rand Modul Dokumentasjon: https://erlang.org/doc/man/rand.html
- EXS1024 Bibliotek: https://hex.pm/packages/exs1024