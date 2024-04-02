---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:37.997085-07:00
description: "Le espressioni regolari (regex) nella programmazione sono schemi utilizzati\
  \ per corrispondere combinazioni di caratteri nelle stringhe. In Elm, come in\u2026"
lastmod: '2024-03-13T22:44:43.340543-06:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) nella programmazione sono schemi utilizzati\
  \ per corrispondere combinazioni di caratteri nelle stringhe. In Elm, come in\u2026"
title: Utilizzo delle espressioni regolari
weight: 11
---

## Cosa & Perché?
Le espressioni regolari (regex) nella programmazione sono schemi utilizzati per corrispondere combinazioni di caratteri nelle stringhe. In Elm, come in altri linguaggi, i programmatori utilizzano le regex per compiti come la validazione dell'input, la ricerca e la sostituzione del testo all'interno delle stringhe, grazie alla loro flessibilità ed efficienza.

## Come fare:
Elm non possiede funzioni regex integrate nella sua libreria di base, richiedendo l'uso di librerie di terze parti per queste operazioni. Una delle scelte popolari per lavorare con le regex è `elm/regex`. Puoi aggiungerlo al tuo progetto usando `elm install elm/regex`.

Ecco come puoi utilizzare `elm/regex` per alcuni compiti comuni:

### 1. Corrispondenza di un modello
Per verificare se una stringa corrisponde a un modello, puoi usare `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Esempio di utilizzo:
isAlphanumeric "Elm2023"     -- Output: True
isAlphanumeric "Elm 2023!"   -- Output: False
```

### 2. Trovare tutte le corrispondenze
Per trovare tutte le occorrenze di un modello all'interno di una stringa, puoi usare `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Esempio di utilizzo:
getWords "Elm è divertente!"  -- Output: ["Elm", "è", "divertente"]
```

### 3. Sostituire il testo
Per sostituire parti di una stringa che corrispondono a un modello, si usa `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Esempio di utilizzo:
replaceElmWithHaskell "Imparare Elm è divertente!"  
-- Output: "Imparare Haskell è divertente!"
```

In questi esempi, `Regex.fromString` viene utilizzato per compilare un modello regex, dove `\b` corrisponde ai limiti delle parole, e `\w` corrisponde a qualsiasi carattere di parola. Gestisci sempre il risultato `Maybe` di `Regex.fromString` per salvaguardarti contro modelli regex non validi, tipicamente usando `Maybe.withDefault`.
