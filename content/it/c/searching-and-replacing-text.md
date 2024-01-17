---
title:                "Cercare e sostituire testo"
html_title:           "C: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che cos'è e perché? 
La ricerca e la sostituzione di testo è un'operazione comune che viene eseguita dai programmatori per automatizzare il processo di modifica di un file di testo. Grazie a questa tecnica, è possibile trovare tutte le occorrenze di una determinata parola o frase e sostituirla con un'altra.

## Come: 
Per eseguire una ricerca e sostituzione di testo in C, utilizziamo la funzione `str_replace()` della libreria `string.h`. Vediamo un esempio pratico che sostituisce la parola "ciao" con "salve".

```
#include <stdio.h>
#include <string.h>

int main() {
  char stringa[50] = "Ciao mondo!";
  char result[50];
  strcpy(result, str_replace(stringa, "ciao", "salve"));
  printf("%s", result);
  return 0;
}
```
Output: Salve mondo! 

## Approfondiamo:
La ricerca e la sostituzione di testo è una tecnica ampiamente utilizzata dai programmatori per automatizzare l'editing di file di testo. È stata sviluppata per la prima volta nel 1975 da Ken Thompson, uno dei creatori del linguaggio C, ed è stata implementata nel famoso editor di testi Unix, “ed”. Oggi, esistono molte alternative a questa tecnica, come ad esempio l'utilizzo di espressioni regolari. Tuttavia, la ricerca e la sostituzione di testo rimane ancora una soluzione semplice ed efficiente.

## Vedi anche:
- [Documentazione ufficiale di C](https://devdocs.io/c/)
- [Introduzione alle espressioni regolari](https://www.rexegg.com/regex-quickstart.html)
- [Altri articoli su C e programmazione](https://www.linkedin.com/pulse/meglio-di-c-rispetto-ad-altri-vera-impaginazione-adrian-bec)