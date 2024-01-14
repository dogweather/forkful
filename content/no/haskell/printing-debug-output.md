---
title:                "Haskell: Utskrift av feilsøkingsmeldinger"
simple_title:         "Utskrift av feilsøkingsmeldinger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive kode kan være både givende og frustrerende. Noen ganger kan det være vanskelig å forstå hvorfor noe ikke fungerer som forventet. Her kommer print-debugging til redning! Ved å printe ut variabler og verdier i koden vår, kan vi få en dypere forståelse av hva som faktisk skjer og feilaktig.

## Hvordan gjøre det

```Haskell
main = do
  let navn = "Ola"
  let alder = 25
  print ("Hei " ++ navn ++ "! Du er " ++ show alder ++ " år gammel.")
```

Dette vil skrive ut:

```
Hei Ola! Du er 25 år gammel.
```

I dette enkle eksempelet kan vi se at vi har en string variabel som heter "navn" og en integer variabel som heter "alder". Ved å bruke print-funksjonen, kan vi kombinere disse to variablene og skrive ut en melding som inkluderer begge. Dette er spesielt nyttig for å sjekke om variabler har riktig verdi og om koden vår kjører som den skal.

## Dypdykk

Når vi printer ut informasjon i koden vår, kan det hjelpe oss med å identifisere feil og løse dem raskere. Det er viktig å bare printe ut de variablene og verdiene som er relevante for problemet vi prøver å løse. Å printe for mye informasjon kan gjøre koden vår rotete og vanskeligere å feilsøke.

Vi kan også bruke print-funksjonen for å forstå hva som skjer i løkkestyrte eller rekursive funksjoner. Ved å printe ut verdier i hver iterasjon, kan vi se hvordan variablene endrer seg og forstå logikken bak koden vår bedre.

## Se også

- [Debugging in Haskell](https://wiki.haskell.org/Debugging)
- [Printing and Formatting Output in Haskell](https://www.cs.nott.ac.uk/~gmh/book.html)
- [Tips for Effective Debugging in Haskell](https://dev.to/m1ghtym0/useful-tips-for-effective-debugging-in-haskell-390a)