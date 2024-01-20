---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å trekke ut substrings er en prosess der vi utleder mindre deler fra en større streng. Programmerere gjør dette for å manipulere og analysere spesifikke segmenter av data innenfor en større datainnsamling.

## Hvordan:

Her er et eksempel på hvordan du trekker ut en substring i Elixir ved hjelp av funksjonen `String.slice/2`.

```elixir
str = "Hei verden"
start_index = 0
end_index = 2

String.slice(str, start_index..end_index)
```

Forventet utskrift av dette programmet er "Hei" som er en del av substrings i vår opprinnelige streng.

## Deep Dive

I eldre programmeringsspråk, som C – hvor strengmanipulasjon ikke alltid var førsteklasses funksjonalitet, kunne det å trekke ut substrings være en meget komplisert oppgave. Men i moderne språk som Elixir er strengmanipulasjon blitt mye mer brukervennlig.

Et alternativ til å bruke `String.slice/2` kan være å bruke `binary_part/3` funksjonen som er mer lav-nivå og krever litt mer kunnskap om binære.

```elixir
str = "Hei verden"
start_index = 0
length = 3

:binary.part(str, start_index, length)
```

`String.slice/2` og de fleste andre strengfunksjoner i Elixir bruker faktisk `binary_part/3`. Dette ble implementert for å gi en mer praktisk måte å håndtere Unicode-strenger og sekvenser i Elixir.

## Se Også

Du kan sjekke ut den offisielle Elixir-dokumentasjonen for mer informasjon om strengfunksjoner:

- `String.slice/2`: https://hexdocs.pm/elixir/String.html#slice/2
- `:binary.part/3`: https://erlang.org/doc/man/binary.html#part-3

Vet også at funksjoner i `:binary` modulen er del av Erlang, som Elixir bygger på og harmoniserer godt med. Elixir er faktisk bare en syntaks, som kompileres ned til Erlangs navnerom.