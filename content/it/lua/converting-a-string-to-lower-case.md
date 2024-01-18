---
title:                "Convertire una stringa in minuscolo"
html_title:           "Lua: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Convertire una stringa in minuscolo è un'operazione comune per i programmatori. Consiste nel cambiare tutte le lettere maiuscole presenti in una stringa con le loro corrispondenti lettere minuscole. Questo è spesso fatto per uniformare il formato delle stringhe e facilitare la comparazione tra di esse.

## Come:

```Lua
-- Esempio di conversione stringa in minuscolo
local stringa = "CIAO, COME STAI?"
print(string.lower(stringa))

-- Output: ciao, come stai?
```

## Approfondimento:

La necessità di convertire una stringa in minuscolo è stata accentuata dall'introduzione dei computer e dei sistemi di elaborazione dei testi. Prima di questo, l'uso delle maiuscole era riservato solo ad alcune circostanze, come l'inizio di una frase o il nome di una persona. Con l'avvento del mondo digitale, invece, è comune utilizzare le maiuscole per enfatizzare parole o frasi, ma questo può causare problemi quando si confrontano le stringhe. In passato, la conversione da maiuscolo a minuscolo era spesso fatto manualmente, ma oggi esistono comandi e funzioni delle librerie di programmazione che rendono questo compito più semplice per i programmatori.

## Vedi anche:

- [Funzione string.lower (Lua)](https://www.lua.org/pil/20.2.html)
- [Confronto tra stringhe (Lua)](https://www.lua.org/pil/3.3.html)