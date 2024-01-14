---
title:    "Elixir: Å søke og erstatte tekst"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med tekstbehandling og programmering, har du kanskje kommet over utfordringen med å bytte ut ord eller setninger med andre. Dette kan være en tidkrevende oppgave, spesielt hvis du må gjøre det manuelt i en stor tekstfil. Men med Elixir trenger du ikke å bekymre deg for dette lenger! Ved hjelp av noen få enkle funksjoner kan du enkelt søke og erstatte tekst uten å måtte gjøre det manuelt.

## Hvordan

Koding av søk og erstatning i Elixir er veldig enkelt og intuitivt. For å søke og erstatte tekst, trenger du bare å bruke funksjonen `String.replace/3`. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Elixir
text = "Hei verden!"
ny_text = String.replace(text, "verden", "Elixir")
IO.puts ny_text
```

Dette vil skrive ut `Hei Elixir!` i terminalen. Som du kan se, tar funksjonen tre argumenter: teksten du vil søke i, teksten du vil erstatte og teksten du vil erstatte den første teksten med. Det er også viktig å merke seg at denne funksjonen bare erstatter den første forekomsten av teksten du søker etter. Hvis du vil erstatte alle forekomster, kan du bruke funksjonen `String.replace/4` og legge til et fjerde argument som angir antall forekomster som skal erstattes.

## Dykk dypere

Det finnes flere måter å søke og erstatte tekst på i Elixir, avhengig av dine behov. Du kan for eksempel bruke funksjonen `String.replace!/3` for å kaste en feilmelding hvis teksten du søker etter ikke finnes i teksten du søker i. Det er også mulig å bruke regulære uttrykk for å søke etter mer komplekse mønstre i teksten.

Hvis du vil gå enda dypere, kan du utforske biblioteker som Regex eller ExRegex for å få enda mer avanserte funksjoner for søk og erstatning.

## Se også

- [Elixir Docs: String module](https://hexdocs.pm/elixir/String.html)
- [Elixir Docs: Regex module](https://hexdocs.pm/elixir/Regex.html)
- [ExRegex: Regular expressions library for Elixir](https://github.com/elixyticsco/ex_regex)