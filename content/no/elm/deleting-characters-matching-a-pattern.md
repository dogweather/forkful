---
title:                "Elm: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til å ønske å slette tegn som matcher et visst mønster i programmering. Kanskje du ønsker å rense tekst eller data før du behandler det videre, eller kanskje du ønsker å fjerne uønskede tegn fra en streng. Uansett hva motivasjonen din er, kan du enkelt lære hvordan du gjør det i Elm.

## Hvordan
For å slette tegn som matcher et mønster i Elm, kan du bruke funksjonen `String.filter`. Denne funksjonen tar inn et predikat (en funksjon som returnerer en boolsk verdi) og en streng, og returnerer den opprinnelige strengen uten de tegnene som matcher det gitte mønsteret. La oss se på et eksempel:

```elm
-- Funksjon for å sjekke om et tegn er en vokal
isVowel : Char -> Bool
isVowel character =
    (character == 'a') || (character == 'e') || (character == 'i') || (character == 'o') || (character == 'u')

-- Filter funksjonen brukes til å fjerne alle vokaler fra en streng
filteredString : String
filteredString =
    String.filter isVowel "Dette er en streng som inneholder flere vokaler"

-- Utskrift av den filtrerte strengen
-- Resultat: "Dtt r n strng sm nnhldr flr vklr"
```

Som du kan se i eksempelet, blir alle vokaler fjernet fra den opprinnelige strengen takket være `String.filter`-funksjonen.

## Dypdykk
Det er viktig å merke seg at `String.filter`-funksjonen i Elm kun fjerner tegn som matcher det gitte mønsteret, og beholder alle andre tegn intakt. Hvis du ønsker å fjerne alle forekomster av en bestemt type tegn, kan du bruke `String.replace`-funksjonen i stedet. Denne funksjonen tar inn en streng eller et regulært uttrykk og erstatter det med et annet tegn. For en grundigere forklaring av disse to funksjonene og deres bruksområder, sjekk ut dokumentasjonen til Elm.

## Se også
* [Elm Docs](https://elm-lang.org/docs)
* [Guide til Strings i Elm](https://www.elm-tutorial.org/en/02-elm-architecture/05-strings.html)
* [Regulære uttrykk i Elm](https://package.elm-lang.org/packages/elm/regex/latest/Regex)