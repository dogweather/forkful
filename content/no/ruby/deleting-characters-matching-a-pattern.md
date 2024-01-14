---
title:                "Ruby: Sletting av tegn som samsvarer med et mønster"
simple_title:         "Sletting av tegn som samsvarer med et mønster"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger når vi jobber med tekststrenger i Ruby, kan det være nyttig å kunne fjerne visse tegn som matcher et bestemt mønster. Dette kan være nyttig når vi ønsker å rense data eller formatere tekst på en spesifikk måte. Å kunne slette tegn som matcher et mønster er en viktig ferdighet for alle Ruby-programmerere å ha.

## Hvordan gjøre det

For å slette tegn som matcher et mønster i Ruby, kan vi bruke metoden `gsub` som står for "global substitution". Denne metoden tar to argumenter: mønsteret vi vil matche og hva vi vil erstatte det med. La oss si vi har en tekststreng som består av tall og vi vil fjerne alle tallmønstre fra strengen. Dette kan gjøres på følgende måte:

```Ruby
string = "Det er 1 uke igjen til foreldremøtet"

ny_string = string.gsub(/\d/, "")

puts ny_string # Output: Det er uke igjen til foreldremøtet
```

Vi definerer et mønster ved hjelp av regulære uttrykk. I dette tilfellet brukte vi `\d` som representerer alle tall i strengen. Deretter erstatter vi alle forekomster av dette mønsteret med et tomt tegn. Dermed vil alle tall bli fjernet fra den opprinnelige strengen.

Vi kan også endre på hvordan vi vil erstatte mønsteret med ved å bruke blokker i `gsub`-metoden. For eksempel, la oss si at vi vil erstatte alle tallmønstre med det samme tallet i motsatt rekkefølge. Dette kan gjøres som følgende:

```Ruby
string = "Jeg elsker å kode i Ruby"

ny_string = string.gsub(/\w+/) { |match| match.reverse }

puts ny_string # Output: geJ reksle å edok i ybuR
```

Her brukte vi en blokk og `reverse`-metoden for å bytte om på bokstavene i hvert matchende ord. Det er viktig å huske at i dette eksemplet, erstattet vi hvert matchende ord med resultatet av blokken.

## Dypdykk

For å få en bedre forståelse av hvordan `gsub`-metoden fungerer, kan vi ta en titt på noen av de andre parameterene den tar. I tillegg til mønsteret og erstatningen, kan vi også inkludere en valgfri tredje parameter for å spesifisere hvor mange ganger vi vil erstatte mønsteret. For eksempel, la oss si at vi bare vil fjerne det første tallet i strengen vår, uavhengig av hvor mange tallet er i strengen. Dette kan gjøres som følgende:

```Ruby
string = "123, 456, 789"

ny_string = string.gsub(/\d/, "", 1)

puts ny_string # Output: 23, 456, 789
```

Vi inkluderte tallet 1 som tredje parameter, noe som betyr at metoden bare vil fjerne det første matchende tallet i strengen.

Vi kan også bruke `gsub!`-metoden for å endre den opprinnelige strengen i stedet for å lage en ny. Dette kan være nyttig når vi ønsker å endre på en streng uten å måtte lagre den i en variabel. Fordi `gsub!`-metoden endrer den opprinnelige strengen, må vi være forsiktige med å bruke den.

## Se Også

- [Ruby regex](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby String documentation](https://ruby-doc.org/core-2.6/String.html#method-i-gsub)
- [Regulære uttrykk i Ruby](https://www.ruby-lang.org/no/documentation/regexp/)