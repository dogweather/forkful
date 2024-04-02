---
date: 2024-01-26 00:51:30.468650-07:00
description: "\xC5 h\xE5ndtere feil betyr \xE5 skrive kode som kan takle at ting ikke\
  \ g\xE5r som planlagt. Programmerere gj\xF8r det for \xE5 hindre krasj og for \xE5\
  \ forsikre seg om at\u2026"
lastmod: '2024-03-13T22:44:40.450906-06:00'
model: gpt-4-1106-preview
summary: "\xC5 h\xE5ndtere feil betyr \xE5 skrive kode som kan takle at ting ikke\
  \ g\xE5r som planlagt. Programmerere gj\xF8r det for \xE5 hindre krasj og for \xE5\
  \ forsikre seg om at\u2026"
title: "Feilh\xE5ndtering"
weight: 16
---

## Hva & Hvorfor?

Å håndtere feil betyr å skrive kode som kan takle at ting ikke går som planlagt. Programmerere gjør det for å hindre krasj og for å forsikre seg om at programmene deres kan komme seg elegant gjennom problemer når Murphy’s lov slår til.

## Hvordan:

I Elixir bruker vi ofte mønstermatching og `case`-setningen for å behandle ulike utfall, inkludert feil.

```elixir
defmodule Eksempel do
  def divider(a, b) do
    case b do
      0 -> {:error, "Kan ikke dele med null."}
      _ -> {:ok, a / b}
    end
  end
end

# Vellykket divisjon
{:ok, resultat} = Eksempel.divider(10, 2)
IO.puts("10 / 2 er #{resultat}")

# Forsøk på å dele med null
{:error, årsak} = Eksempel.divider(10, 0)
IO.puts("Feil: #{årsak}")
```

Eksempel på utdata:
```
10 / 2 er 5.0
Feil: Kan ikke dele med null.
```

Når du kjører denne Elixir-koden, vil du enten få et vellykket divisjon eller en feilmelding, avhengig av inndataene dine. Ingen kræsj her!

## Dypdykk

I gamle dager handlet feilhåndtering oftere om å sjekke returverdier. Med Elixirs funksjonelle røtter har vi imidlertid mønstermatching og merkede tupler, som `{:ok, verdi}` eller `{:error, årsak}`, som er mer elegante.

Det er andre måter å håndtere feil i Elixir:

- **Elixirs `try` og `rescue`**, som ligner på det tradisjonelle `try-catch` i imperativt språk, men brukes sjeldnere på grunn av Elixirs preferanse for eksplisitthet.
- **Supervisorer og GenServers**, en del av Elixirs OTP-rammeverk, som handler mer om feiltoleranse. De overvåker prosessen til koden din, klar til å starte den på nytt hvis ting går galt.

Når det gjelder implementering bygger Elixir på Erlangs robusthet. Det behandler feil som bare en annen type melding som skal håndteres med all mønstermatching og funksjonelle finesser.

## Se også

For videre lesing om feilhåndtering i Elixir, sjekk ut:

- Elixirs offisielle guide om [feilhåndtering](https://elixir-lang.org/getting-started/try-catch-and-rescue.html).
- Lær mer om [prosesser og OTP](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html).
- Elixir Forum er alltid et godt sted å stille spørsmål: [https://elixirforum.com](https://elixirforum.com).
