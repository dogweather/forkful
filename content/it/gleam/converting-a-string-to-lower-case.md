---
title:                "Gleam: Convertire una stringa in minuscolo"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo può essere utile in tante situazioni diverse, come ad esempio per facilitare il confronto tra stringhe o per rendere il testo uniforme.

## Come fare

Per convertire una stringa in minuscolo in Gleam, possiamo utilizzare il metodo `String.to_lowercase` e fornire come argomento la stringa da convertire. Possiamo poi visualizzare il risultato utilizzando la funzione `IO.println` come mostrato nell'esempio seguente:

```Gleam
my_string = "STRINGA IN MAIUSCOLO"
IO.println(String.to_lowercase(my_string))
```

Questo codice stamperà a schermo "stringa in maiuscolo".

## Approfondimento

Il metodo `String.to_lowercase` si basa sull'algoritmo Unicode standard per la conversione di una stringa in minuscolo. Ciò significa che funziona correttamente anche per caratteri speciali o di diverse lingue.

Ad esempio, in alcune lingue come il tedesco, alcune lettere quando convertite in minuscolo possono avere dei caratteri speciali aggiuntivi, come accenti o puntini. Il metodo `String.to_lowercase` gestisce automaticamente questi casi e restituisce il risultato corretto.

Inoltre, se vogliamo convertire solo la prima lettera di una stringa in minuscolo, possiamo utilizzare il metodo `String.to_lowercase_first`, che restituirà una nuova stringa con la prima lettera convertita ma mantenedo le altre lettere inalterate.

## Vedi anche

- Documentazione ufficiale di Gleam su come lavorare con le stringhe: [https://gleam.run/book/core-modules#strings](https://gleam.run/book/core-modules#strings)

- Altri esempi di conversione di stringhe in minuscolo in Gleam: [https://github.com/gleam-lang/gleam-examples/blob/master/string_manipulation/string_case.gleam](https://github.com/gleam-lang/gleam-examples/blob/master/string_manipulation/string_case.gleam)

- Blog post su come utilizzare l'algoritmo Unicode per la conversione di stringhe in minuscolo: [https://unicode.org/faq/casemap_charprop.html](https://unicode.org/faq/casemap_charprop.html)