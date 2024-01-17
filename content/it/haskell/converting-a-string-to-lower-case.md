---
title:                "Convertire una stringa in minuscolo"
html_title:           "Haskell: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Convertire una stringa in minuscolo è un'operazione comune nella programmazione, che permette di rendere uniforme il testo all'interno di un programma. Questo può essere utile per l'elaborazione di input dell'utente, la comparazione di stringhe o la creazione di interfacce più intuitive.

## Come fare:
```Haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase str = map toLower str
```
Esempio di input: "Ciao mondo"
Output: "ciao mondo"

## Deep Dive:
La conversione di stringhe in minuscolo ha origine dal concetto di codifica dei caratteri, dove ogni lettera è rappresentata da un codice numerico univoco. In passato, diverse codifiche di caratteri portavano a risultati diversi quando si tentava di convertire una stringa in minuscolo. Oggi, la maggior parte delle implementazioni di questa operazione utilizzano il formato UTF-8 per garantire la corretta conversione del testo.

Ci sono anche alternative per la conversione di stringhe in minuscolo, come l'utilizzo di espressioni regolari o la creazione di una funzione personalizzata. Tuttavia, la funzione "toLower" del modulo Data.Char di Haskell è uno dei modi più semplici e affidabili per ottenere questo risultato.

## Vedi anche:
- Documentazione del modulo Data.Char di Haskell: https://hackage.haskell.org/package/base/docs/Data-Char.html
- Tutorial su come gestire le stringhe in Haskell: https://blog.fossasia.org/handling-strings-in-haskell/