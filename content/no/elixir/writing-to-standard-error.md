---
title:                "Skrive til standardfeil"
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Skrive til standardfeil (stderr) er å sende feilmeldinger og diagnostikk separat fra hoveddatautstrømmen. Programmerere gjør dette for å spore og håndtere feil effektivt uten å blande det med vanlig output.

## How to:
Elixir gir en enkel måte å skrive til stderr på. Bruk `IO.warn/1` for advarsler eller `IO.puts/2` for å spesifisere stderr direkte.

```elixir
# Skriver en advarsel til stderr
IO.warn("Advarsel: Noe gikk ikke som forventet!")

# Skriver en spesifikk melding til stderr
IO.puts(:stderr, "Feil: Ugyldig inndata.")
```

Forventet output til stderr:
```
Advarsel: Noe gikk ikke som forventet!
Feil: Ugyldig inndata.
```

## Deep Dive
Historisk sett har separate strømmer for output (stdout) og errors (stderr) latt programmer skille mellom data og feil. Alternative metoder inkludere logging-biblioteker som kan tilby mer fleksibilitet. Elixir implementerer stderr gjennom Erlang sin :io-modul, og fungerer på alle systemer som støtter Erlang VM.

## See Also
- Elixir's offisielle dokumentasjon på `IO`: https://hexdocs.pm/elixir/IO.html
- Erlang's :io modul for dypere systeminteraksjoner: http://erlang.org/doc/man/io.html
- Innføring i Logger, Elixir's logging-bibliotek: https://hexdocs.pm/logger/Logger.html