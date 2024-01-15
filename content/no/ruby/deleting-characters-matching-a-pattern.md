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

## Hvorfor

Noen ganger vil du kanskje trenge å fjerne bestemte tegn eller bokstaver fra en tekststreng. Dette kan være nyttig hvis du for eksempel ønsker å rense data eller filtrere ut uønskede tegn.

## Hvordan gjøre det

```Ruby
tekst = "Rubyyy"

tekst.gsub!(/y/, "") 

puts tekst
```

Dette vil resultere i at teksten blir "Rub".

## Dypdykk

Ruby har en innebygd metode kalt `gsub` som står for "global substitution". Denne metoden tar to argumenter, et mønster som skal matches og hva det skal erstattes med. Når du bruker `gsub!` vil endringen skje i selve variabelen, mens `gsub` returnerer en ny kopi av teksten med endringen.

Du kan også bruke regulære uttrykk i `gsub` for å slette flere tegn eller bokstaver basert på et mønster. For eksempel, hvis du vil fjerne alle tall fra en tekststreng, kan du bruke `tekst.gsub!(/\d/, "")`, hvor `\d` representerer et tall.

## Se også

- [RegExr](https://regexr.com/) - et nyttig verktøy for å teste og lage regulære uttrykk
- [Ruby String dokumentasjon](https://ruby-doc.org/core-2.7.2/String.html#method-i-gsub) - mer informasjon om `gsub` metoden og andre nyttige metodene for strings i Ruby.