---
title:    "Elixir: Skriving til standardfeil"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor
Å skrive til standardfeil kan være en nyttig feilsøkingsmetode for å oppdage potensielle problemer eller uønsket oppførsel i Elixir-kode. Det kan også hjelpe med å identifisere og isolere feil i større prosjekter.

# Hvordan
For å skrive til standardfeil i Elixir, kan du bruke funksjonen `IO.puts/2` med argumentet `:stderr` for å skrive til standard error stream. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Elixir
# Skriver en melding til standard error stream
IO.puts(:stderr, "Dette er en feilmelding")

# Skriver en variabelverdi til standard error stream
feil = "Ingen tilkobling til server"
IO.puts(:stderr, feil)
```

Denne koden vil skrive ut følgende til standard error stream:

```
Dette er en feilmelding
Ingen tilkobling til server
```

Du kan også bruke `IO.inspect/2` funksjonen til å inspisere verdier og utskrive dem til standardfeil for feilsøking. Her er et eksempel på hvordan dette kan gjøres:

```Elixir
# Variabel som inneholder et komplekst struct
bruker = %{
  navn: "Johannes",
  alder: 25,
  interesser: ["programmering", "musikk", "friluftsliv"]
}

# Skriver ut structet til standard error stream
IO.inspect(:stderr, bruker)
```

Dette vil skrive ut følgende til standard error stream:

```
%{alder: 25, interesser: ["programmering", "musikk", "friluftsliv"], navn: "Johannes"}
```

# Dypdykk
Når du skriver til standardfeil, kan du også kontrollere nivået på meldingene ved hjelp av `Logger` modulen i Elixir. Dette lar deg spesifisere forskjellige nivåer som `:debug`, `:info` og `:warn`, som deretter kan filtreres og styres i loggfiler.

I tillegg kan du også bruke `raise/1` funksjonen for å løfte en unntaksmelding og utskrive den til standard error stream. Dette kan være nyttig når du vil få oppmerksomhet om en spesiell feilsituasjon i koden din.

# Se også
- [IO-modulen i Elixir](https://hexdocs.pm/elixir/IO.html)
- [Logger-modulen i Elixir](https://hexdocs.pm/logger/Logger.html)
- [Raise/1 funksjonen i Elixir](https://hexdocs.pm/elixir/Kernel.html#raise/1)