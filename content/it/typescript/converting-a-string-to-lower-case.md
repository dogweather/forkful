---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?

Trasformare una stringa minuscola è un'operazione che converte tutti i caratteri alfanumerici di una stringa in minuscolo. Lo facciamo per normalizzare i dati e renderli uniformi, aiutando a prevenire errori durante il confronto delle stringhe.

## Come Fare:

Ecco un esempio semplice di come trasformare una stringa in minuscolo in TypeScript.

```TypeScript
let stringaMaiuscola: string = "CIAO MONDO!";
let stringaMinuscola: string = stringaMaiuscola.toLowerCase();

console.log(stringaMinuscola);
// Risultato: "ciao mondo!"
```

In questo esempio "CIAO MONDO!" viene convertito in "ciao mondo!" usando il metodo `toLowerCase()`.

## Approfondimento

Nell'ambito della programmazione, la conversione di una stringa in minuscolo è un'operazione comune che esiste da molto tempo nelle varie lingue. In TypeScript, viene utilizzato il metodo nativo di JavaScript `toLowerCase()`.

Ci sono alternative a `toLowerCase()`, come `toLocaleLowerCase()`, che rispetta le regole di localizzazione per i caratteri specifici di una lingua. Ad esempio, il tedesco ha un carattere "ß" che diventa "SS" quando è in maiuscolo, ma rimane "ß" quando è in minuscolo.

Entrambi questi metodi applicano la conversione minuscola agli elementi della stringa in posto. In altre parole, non si limitano a restituire una nuova stringa convertita, ma modificano anche la stringa originale.

## Link Utili

Per ulteriori dettagli, consultare le seguenti fonti:

- [String.prototype.toLowerCase() - JavaScript | MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [TypeScript - String toLowerCase() Method - Tutorialspoint](https://www.tutorialspoint.com/typescript/typescript_string_tolowercase.htm)
- [String.prototype.toLocaleLowerCase() - JavaScript | MDN](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)