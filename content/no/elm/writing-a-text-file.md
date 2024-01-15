---
title:                "Skriver en tekstfil"
html_title:           "Elm: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan være en nyttig ferdighet for alle som ønsker å lære å kode i Elm. Det er en enkel måte å lagre og organisere data på, og det kan være nyttig i forskjellige programmeringsprosjekter.

## Slik gjør du det

Det første du må gjøre er å opprette en ny tekstfil med ".elm" som filtype. Deretter kan du begynne å skrive koden din. Her er et enkelt eksempel på hvordan du kan skrive en tekstfil med en liste over tall:

```Elm
-- Definerer en liste med tall
numbers = [1, 2, 3, 4, 5]

-- Åpne en fil for å skrive
file = File.write "min_tekstfil.elm" (String.join "\n" (List.map String.fromInt numbers))

-- Sjekk om filen ble opprettet
case file of
    Err error ->
        Debug.log "Det skjedde en feil!" error

    Ok _ ->
        Debug.log "Tekstfil opprettet suksessfult!"
```

Dette eksempelet viser hvordan du kan bruke funksjoner som "File.write" og "String.join" for å skrive en tekstfil med en liste over tall. Som et resultat vil du få en fil kalt "min_tekstfil.elm" som ser slik ut:

```
1
2
3
4
5
```

## Dypdykk

Når du skriver en tekstfil i Elm, er det viktig å vite at filen vil bli lagret i en tekstbasert UTF-8 format. Dette betyr at du kan skrive tekst på mange forskjellige språk, og at filen vil bli lagret riktig.

Noen viktige ting å huske når du skriver en tekstfil i Elm:

- Bruk funksjoner som "File.write" og "String.join" for å skrive data til filen.
- Bruk "Text.encode" funksjonen for å sørge for at tekstfilen blir lagret i riktig format.
- Sørg for at du har god forståelse av hvordan filstier fungerer i Elm, slik at du kan lagre og åpne filen på riktig sted.

## Se også

- [Elm Official Documentation](https://guide.elm-lang.org/)
- [Elm Tutorials](https://elmprogramming.com/)
- [Elm Community Packages](https://package.elm-lang.org/)