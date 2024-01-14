---
title:    "Elm: Å skrive tester"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Writing tests can seem tedious and time-consuming, but it is an essential part of good programming practice. Not only do tests ensure that your code runs smoothly and as expected, but it also helps to catch any potential bugs and errors early in the development process.

## Slik gjør du det

Å skrive tester i Elm er enkelt og intuitivt, og det er ingenting å være redd for. La oss se på et eksempel på en testfunksjon som sjekker om en liste med tall er riktig sortert:

```Elm
sortertListe : List number -> Bool
sortertListe tall =
    tall
        |> List.sort
        |> List.equals tall
```

Her har vi definert en funksjon som tar inn en liste med tall og sjekker om den er sortert riktig ved å først sortere listen og deretter sammenligne den med den originale listen. Vi kan nå enkelt kalle på denne testfunksjonen og se om den returnerer `True` eller `False`:

```Elm
sortertListe [1, 3, 5, 2] -- False
sortertListe [1, 2, 3, 5] -- True
```

Det er viktig å legge merke til at testene bør dekke alle mulige tilfeller, også de uventede. I tillegg bør du inkludere kommentarer i koden din for å gjøre det enklere å forstå og vedlikeholde testene.

## Dykk dypere

Det er mange ulike tester som kan skrives i Elm, avhengig av hva slags type program du skriver. Du kan for eksempel skrive tester for funksjoner, grensesnitt eller integrasjonsnivå. Det er også mulig å kjøre enkelte tester automatisk ved å bruke moduler som [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/), som hjelper deg med å generere data og kjøre tester automatisk.

Det er viktig å huske at tester også er en del av koden, og derfor bør de skrives på en ryddig og effektiv måte. Dette gjør det enklere å feilsøke og oppdage eventuelle feil eller mangler i koden.

## Se også

- [Elm Test Documentation](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Testing in Elm](https://medium.com/@ProperIntro/testing-in-elm-f029895dc670)
- [Why Write Tests in Elm?](https://medium.com/@joelq/why-write-tests-in-elm-5f2065f6a4b6)