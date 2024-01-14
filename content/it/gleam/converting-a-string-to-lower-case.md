---
title:                "Gleam: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

**Perché:**

Il processo di conversione di una stringa in lettere minuscole è un'operazione comune nella programmazione. Ciò è utile per uniformare i dati in un sistema e semplificare il confronto e la ricerca all'interno delle stringhe.

**Come Fare:**

Per convertire una stringa in lettere minuscole in Gleam, è possibile utilizzare il metodo `string.to_lower()` come mostrato nell'esempio seguente:

```Gleam
let stringa = "CIAO AMICI"
let stringa_minuscola = stringa.to_lower()
```
Output: "ciao amici"

È importante notare che il metodo `to_lower()` converte solo le lettere ASCII in minuscolo, quindi le lettere accentate o caratteri speciali potrebbero non essere convertiti correttamente. Se si utilizzano stringhe contenenti caratteri non ASCII, è possibile utilizzare il metodo `to_utf8_lowercase()` per una conversione completa di tutti i caratteri in lettere minuscole.

**Approfondimento:**

La conversione di una stringa in lettere minuscole può sembrare un'operazione semplice, ma è importante comprendere come funzionano le lettere nei diversi sistemi di codifica. In UTF-8, per esempio, ci sono due tipi di lettere minuscole: quelle che possono essere rappresentate con un unico byte e quelle che richiedono più byte. Ciò significa che il metodo `to_lower()` potrebbe dover spacchettare un carattere in più byte per convertire correttamente le lettere minuscole nei sistemi di codifica multi-byte.

**Vedi Anche:**
- Documentazione ufficiale di Gleam sulla conversione delle stringhe: https://gleam.run/doc/types/string
- Esempi di utilizzo del metodo `to_lower()` su Codewars: https://www.codewars.com/kata/5728203b7fd8f24ebe0001f7/solutions/gleam