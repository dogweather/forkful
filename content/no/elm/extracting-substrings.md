---
title:    "Elm: Uthenting av delstrenger"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Hvorfor

Å ekstrahere substringer, eller delstrenger, fra en streng er ofte nødvendig når man jobber med tekstbehandling eller dataanalyse. Ved å uteksedere kun en del av en streng, kan man få tilgang til spesifikke deler av informasjonen og behandle den separat. Dette kan være spesielt nyttig når man ønsker å filtrere data eller manipulere tekstelementer.

# Hvordan

Først og fremst må vi importere biblioteket `String` for å kunne bruke de innebygde funksjonene for å ekstrahere substringer. Deretter kan vi bruke funksjonen `slice` til å angitte start- og sluttposisjonen for den delen av strengen vi ønsker å ekstrahere.

```Elm
import String exposing (slice)

tekst = "Dette er en tekststreng"

delstreng = slice 6 8 tekst

-- output: "er"
```

I dette eksemplet har vi ekstrahert delen av teksten mellom posisjon 6 og 8, og fått ut substrengen "er". Det er også mulig å bruke `slice` til å ekstrahere fra slutten av strengen ved å bruke negative tall.

```Elm
tekst = "Dette er en tekststreng"

delstreng = slice -8 -1 tekst

-- output: "tekststre"
```

Man kan også angi kun startposisjon, og dermed ekstrahere alt etter denne.

```Elm
tekst = "Dette er en tekststreng"

delstreng = slice 11 tekst

-- output: "tekststreng"
```

# Dypdykk

I tillegg til `slice`-funksjonen finnes det også andre måter å ekstrahere substringer på i Elm. En annen nyttig funksjon er `left`, som lar deg skrive ut den angitte delen fra venstre side av strengen.

```Elm
import String exposing (left)

tekst = "Dette er en tekststreng"

delstreng = left 5 tekst

-- output: "Dette"
```

Det finnes også `right`-funksjonen som gjør det samme på høyre side av strengen.

```Elm
import String exposing (right)

tekst = "Dette er en tekststreng"

delstreng = right 9 tekst

-- output: "tekststreng"
```

En annen nyttig funksjon er `split`, som lar deg dele en streng opp i en liste ved å angi et bestemt tegn.

```Elm
import String exposing (split)

tekst = "1, 2, 3, 4, 5"

delstrenge = split ", " tekst

-- output: ["1", "2", "3", "4", "5"]
```

Dette kan være nyttig når man ønsker å behandle hver del av en string separat.

# Se også

- [Offisiell dokumentasjon for `String`-modulen](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm i praksis - ekstrahering av substringer](https://medium.com/elm-i-praksis/ekstrahering-av-substringer-med-elm-be900f6c0f75)
- [Eksempelprosjekt med bruk av `slice` i Elm](https://github.com/Elm-i-praksis/slice-example)