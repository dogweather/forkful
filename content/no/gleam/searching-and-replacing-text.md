---
title:    "Gleam: Søke og erstatte tekst"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Enten du er en nybegynner eller en erfaren programmerer, er det ofte behov for å kunne endre tekst i batch. Dette kan være for å raskt korrigere feil eller for å gjøre større endringer i en kodebase. Med Gleam har du muligheten til å gjøre dette enkelt og effektivt, noe som vil hjelpe deg med å spare tid og unngå slitsomt manuelt arbeid.

# Hvordan gjøre det med Gleam

Det første trinnet for å søke og erstatte tekst med Gleam, er å importere biblioteket "Text.Replace". Dette vil gi oss de nødvendige funksjonene for å gjøre endringer i tekst.

```Gleam
import Text.Replace
```

Deretter kan du bruke funksjonen "replace" til å søke etter en gitt streng og erstatte den med en annen.

```Gleam
let nyTekst = Text.Replace.replace("Hei, verden!", "Hei", "Hallo")

// Resultat: "Hallo, verden!"
```

Dette er bare et enkelt eksempel, men funksjonen "replace" kan også ta i bruk regulære uttrykk for å søke etter mønstre i en tekst.

```Gleam
let resultat = Text.Replace.replaceRegex("Gleam er kult!", Regex.compile("\\w+"), fun(ord) {
  "Kul" ++ ord ++ "?"
})

// Resultat: "Kul Gleam? Kul er? Kul kult?"
```

# Dykk dypere ned

Å søke og erstatte tekst kan virke enkelt, men det er faktisk en veldig nyttig funksjon å kunne beherske. Med Gleam kan du også ta i bruk forskjellige teknikker og metoder for å tilpasse søket og erstatningen basert på dine behov. Du kan for eksempel bruke funksjonen "replaceFirst" for å bare erstatte den første forekomsten av en tekst eller bruke "replaceAllAcc" for å jobbe med akkumulatorer og håndtere større datasett.

# Se også

- [Offisiell dokumentasjon for "Text.Replace" biblioteket](https://gleam.run/modules/text#replace)
- [Tutorial for søke og erstatte tekst med Gleam](https://www.youtube.com/watch?v=H6Jy4DYOh0k)
- [Eksempler på Gleam kode for å søke og erstatte tekst](https://github.com/search?q=language%3Agleam+replace)