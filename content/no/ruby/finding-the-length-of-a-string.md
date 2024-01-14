---
title:                "Ruby: Å finne lengden av en streng"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor?
Hvorfor skulle noen ønske å finne lengden på en streng? Vel, det er en viktig del av programmering! Å kunne få tilgang til lengden på en streng er nyttig når man skal manipulere tekst eller håndtere brukerinndata. Det er også en grunnleggende ferdighet som alle programmerere bør ha.

## Hvordan gjør man det?
I Ruby er det enkelt å finne lengden på en streng. Alt du trenger å gjøre er å bruke metoden `length` på strengen din. La oss si at vi har strengen "Hei, verden!" og vil finne lengden på den. Dette er hvordan vi gjør det i Ruby:

```Ruby
streng = "Hei, verden!"
puts streng.length
```

Outputen fra dette vil være:

```Ruby
13
```

Det betyr at strengen vår består av 13 tegn, inkludert mellomrom og tegnsetting. En annen måte å finne lengden på en streng er ved å bruke metoden `size`. Begge metodene fungerer på samme måte og gir samme resultat.

## Dykk dypere
Nå som du vet hvordan du finner lengden på en streng, la oss ta en rask titt på hva som egentlig skjer bak kulissene. Når vi kaller på `length` eller `size` metoden på en streng, så teller Ruby antallet tegn i strengen og returnerer dette som et heltall. Dette betyr at metoden `length` eller `size` kan brukes på alle typer objekter, ikke bare strenger.

## Se også
- [Ruby Strings](https://www.rubyguides.com/2019/12/ruby-strings/)
- [Ruby String Class](https://ruby-doc.org/core-2.7.0/String.html)
- [Learn Ruby - Strings](https://www.learnruby.org/en/strings)