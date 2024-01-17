---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "Ruby: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Fjerning av tegn som matcher et mønster er en vanlig måte for programvareutviklere å manipulere og håndtere strenger av tekst i Ruby. Dette kan være nyttig når man ønsker å filtrere ut uønsket informasjon eller endre strenger for å passe til en bestemt format.

## Slik gjør du det:
Følgende eksempler viser hvordan å slette tegn som matcher et mønster ved hjelp av Ruby-programmeringsspråket. I hvert eksempel vil koden bli vist i en kodeblokk markert med ```Ruby```:

### Eksempel 1: Fjern et bestemt tegn fra en streng
```Ruby
string = "Hei verden!"
puts string.delete("e")
```
Output: Hllo world!

### Eksempel 2: Fjern alle vokaler fra en tekst
```Ruby
string = "Dette er en setning"
puts string.delete("aeiouy")
```
Output: Dtt r n stnng

### Eksempel 3: Fjern tall fra en streng
```Ruby
string = "123 Hei 456 verden"
puts string.delete("0-9")
```
Output: Hei verden

## Dykk dypere:
Sletting av tegn basert på et mønster er ikke bare begrenset til Ruby. Det finnes også andre programmeringsspråk som tilbyr lignende funksjonalitet, som Java og Python. I tillegg til å bruke ```delete```-metoden, kan man også slette tegn ved hjelp av regulære uttrykk i Ruby.

## Se også:
* [Ruby dokumentasjon om ```String#delete```metoden](https://ruby-doc.org/core-3.0.2/String.html#method-i-delete)
* [Enkle Python-eksempler på å fjerne tegn basert på et mønster](https://www.geeksforgeeks.org/python-string-methods-set-3-strip-lstrip-rstrip-min-max-maketrans-translate-rel-just-zfill/)