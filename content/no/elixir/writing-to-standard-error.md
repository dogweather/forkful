---
title:    "Elixir: Skriver til standardfeil"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standardfeil er en viktig del av Elixir programmering, spesielt når det kommer til debugging og feilhåndtering. Det lar deg skrive ut feilmeldinger og annen informasjon som kan hjelpe deg med å forstå og løse problemer i koden din.

## Hvordan Du Gjør Det

I Elixir kan du skrive til standardfeil ved å bruke `IO.puts/2` funksjonen og spesifisere `:stderr` som argument. La oss si at vi har en variabel `error` som inneholder en feilmelding. For å skrive ut dette til standardfeil kan vi bruke følgende kodeblokk:

```Elixir 
IO.puts "En feil har oppstått: #{error}", :stderr
```

Dette vil skrive ut feilmeldingen til standardfeil og du kan deretter se den i terminalen din.

## Dykk Dypere

I tillegg til å skrive ut feilmeldinger, kan du også bruke standardfeil for å skrive ut generell informasjon mens du kjører koden din. Dette kan være nyttig når du vil holde deg oppdatert om statusen til programmet ditt. For eksempel kan du bruke `IO.write/2` funksjonen til å skrive til standardfeil uten å legge til en ny linje på slutten. Dette kan være nyttig hvis du ønsker å skrive ut informasjon i en løkke uten å skrive ut en ny linje for hvert trinn.

En annen ting å merke seg er at standardfeil vil bli skrevet ut i rødt i terminalen, noe som gjør det enkelt å identifisere og skille fra annen utdatert informasjon fra programmet ditt.

## Se også

- [Elixir dokumentasjon om IO modulen](https://hexdocs.pm/elixir/IO.html)
- [Elixir dokumentasjon om standard IO](https://hexdocs.pm/elixir/IO.html#module-standard-io)