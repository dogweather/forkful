---
title:    "Ruby: Søking og erstattning av tekst"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en av de mest grunnleggende og viktige funksjonene i programmering. Det lar deg enkelt manipulere og endre store mengder tekst på en effektiv måte. Enten du skal arbeide med tekstfiler, nettsider eller databaser, er det nesten uunngåelig å ikke trenge å søke og erstatte tekst.

## Hvordan

Søke og erstatte tekst i Ruby er enkelt med hjelp av innebygde funksjoner og metoder. La oss ta en titt på et eksempel:

```Ruby
original_text = "Hei, verden!"
modifisert_text = original_text.gsub("Hei", "Hallo")
puts modifisert_text
```

I dette eksempelet erstatter vi "Hei" med "Hallo" og printer det modifiserte resultatet. Output vil være "Hallo, verden!". Du kan også bruke regulære uttrykk for mer avanserte søk og erstatning. Her er et eksempel på å bytte ut alle forekomster av tall med bokstaven "X":

```Ruby
original_tekst = "123456789"
modifisert_tekst = original_tekst.gsub(/\d/, "X")
puts modifisert_tekst
```

Output vil være "XXXXXXXXX". Dette er bare noen få eksempler, men mulighetene er nesten uendelige.

## Dykk dypere

For å få full forståelse av søke og erstatte tekst i Ruby, er det viktig å lære om Regular Expressions og deres syntaks. Regular Expressions lar deg lage mønstre og søke etter tekst som passer disse mønstrene. I tillegg til "gsub" metoden, har Ruby også "sub" og "gsub!" metoder som gir deg enda flere muligheter for å søke og erstatte tekst. Det kan også være nyttig å se på dokumentasjonen for å lære mer om alle de forskjellige alternativene du kan bruke når du søker og erstatter tekst.

## Se også
- [Ruby dokumentasjon](https://www.ruby-lang.org/no/documentation/)
- [En grundig guide til Regular Expressions](https://www.regular-expressions.info/)
- [Gode Ruby øvingsoppgaver for å lære søk og erstatte tekst](https://www.rubyguides.com/2015/06/ruby-string-methods-cheat-sheet/)