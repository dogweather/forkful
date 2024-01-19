---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perchè?
Convertire una stringa in caratteri minuscoli significa trasformare tutti i caratteri alfabetici della stringa in minuscolo. I programmatori lo fanno per rendere il confronto di stringhe indipendente dalle differenze di maiuscole/minuscole.

## Come fare:
Ecco un esempio veloce su come convertire una stringa in minuscolo in Lua:

```Lua
stringa = "CIAO MONDO"
print(stringa:lower())
```

Questo genera:

```Lua
"ciao mondo"
```

## Approfondimento
Historicamente, la funzione `string.lower` è stata introdotta in Lua 4.0 nel 2000 per fornire un modo semplice e efficace per convertire le stringhe in minuscolo. Non esistono alternative native dirette a `string.lower` in Lua, tuttavia è possibile utilizzare regex o cicli per ottenere risultati simili.

I dettagli di implementazione di `string.lower` variano da piattaforma a piattaforma. In generale, la funzione esamina ogni carattere della stringa e, se si tratta di una lettera maiuscola, la converte in minuscolo. I caratteri non alfabetici rimangono inalterati.

## Per saperne di più
Per approfondire quanto discusso in questo articolo, segui questi collegamenti utili:

1. [Documentazione Lua](https://www.lua.org/manual/5.3/manual.html)
2. [Corso di programmazione Lua](https://www.luascript.com/)
3. [Guide di programmazione Lua](https://lua.space/)