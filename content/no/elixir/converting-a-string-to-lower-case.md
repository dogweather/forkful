---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver er prosessen med å endre alle stor bokstaver i en given tekststreng til sine tilsvarende små bokstaver. Programmerere gjør dette for å standardisere data, noe som ofte forenkler søk, sammenligninger og analyse.

## Hvordan:

Eksempler på koding og utdata er som følger:
```Elixir 
String.downcase("HELLO WORLD") 
```
Dette vil returnere: 
```Elixir
"hello world"
```
## Deep Dive:

Å konvertere en streng til små bokstaver er ikke en ny idé og kommer fra en tid da datasystemer var mer case-sensitive enn de er nå. I Elixir er dette implementert gjennom 'String.downcase' funksjonen, men det er også viktig å merke seg at denne funksjonen er avhengig av operativsystemets lokale innstillinger.

Alternativt kan du konvertere en streng til små bokstaver ved å bruke Unicode nedbrekningsalgoritmen. Dette er mer komplekst, men vil gi mer konsistente resultater på tvers av ulike språk og skriftsystemer.

Under implementeringen av `String.downcase`, brukes en nokså enkel tilnærming i Elixir. Funksjonen bruker en innebygd funksjon i Erlang, `:unicode.characters_to_nfc_binary`, for å konvertere hver enkelt stor bokstav til en tilsvarende liten bokstav.

## Se også:

1. Elixir's `String` modul dokumentasjon: [Link](https://hexdocs.pm/elixir/String.html)
2. Mer informasjon om case mapping: [Link](https://www.unicode.org/versions/Unicode7.0.0/ch03.pdf)
3. Djupdykk i Unicode: [Link](http://unicode.org/standard/WhatIsUnicode.html) 
4. Unicode kollapsalgoritmer: [Link](http://www.unicode.org/reports/tr15/) 

Det er ingen 'Konklusjon' seksjon.