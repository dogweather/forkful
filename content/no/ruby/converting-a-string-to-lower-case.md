---
title:                "Ruby: Konvertere en streng til små bokstaver."
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Konvertering av tekst til små bokstaver er en vanlig oppgave i Ruby-programmering. Dette kan være nyttig for å standardisere tekst og forenkle sammenligninger.

## Hvordan

For å konvertere en streng til små bokstaver i Ruby, kan du bruke metoden `downcase`. Her er et eksempel på hvordan du kan bruke denne metoden:

```Ruby
streng = "VELKOMMEN TIL MITT PROGRAM"

puts streng.downcase
```

Dette vil gi følgende output:

```Ruby
velkommen til mitt program
```

## Dypdykk

Det er viktig å merke seg at konvertering til små bokstaver i Ruby bare fungerer for engelsk tekst. Dersom du har tekst med spesielle tegn eller bokstaver fra andre språk, kan du møte på problemer når du skal konvertere til små bokstaver. I disse tilfellene kan du bruke metoden `unicode_normalize` for å håndtere slike utfordringer.

## Se også

* [String Documentation](https://ruby-doc.org/core-3.0.2/String.html)
* [Metoder for tekstbehandling i Ruby](https://www.rubyguides.com/2018/07/ruby-string-methods/)
* [Utforsk flere Ruby-konsepter og metoder](https://www.ruby-lang.org/no/documentation/)