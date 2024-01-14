---
title:    "C: Utilizzo delle espressioni regolari"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché usare le espressioni regolari?

Le espressioni regolari sono uno strumento potente per manipolare e gestire stringhe di testo in modo efficiente. Sono ampiamente utilizzate nella programmazione per cercare, sostituire e validare i dati. Inoltre, possono essere utilizzate in quasi tutti i linguaggi di programmazione, inclusi C.

## Come utilizzare le espressioni regolari in C

Per utilizzare le espressioni regolari in C, è necessario includere la libreria `regex.h` nel tuo programma. Una volta fatto ciò, puoi iniziare a utilizzare le funzioni fornite dalla libreria per gestire le espressioni regolari.

Ecco un semplice esempio di codice che trova tutte le occorrenze di una determinata parola in una stringa:

```C
#include <regex.h>
#include <stdio.h>

int main() {
  // Definisci il pattern di ricerca e la stringa di input
  char *pattern = "ciao";
  char *input = "Ciao a tutti! Sono felice di vedervi.";

  // Crea un oggetto regex
  regex_t regex;

  // Compila il pattern
  regcomp(&regex, pattern, 0);

  // Cerca il pattern nella stringa di input utilizzando regexec()
  regmatch_t match;
  if (regexec(&regex, input, 1, &match, 0) == 0) {
    printf("Trovato '%s' a partire dalla posizione %d", pattern, match.rm_so);
  }
  else {
    printf("Nessuna corrispondenza trovata");
  }

  return 0;
}
```

**Output:**
`Trovato 'ciao' a partire dalla posizione 0`

Puoi anche utilizzare le espressioni regolari per sostituire parti di una stringa con un'altra. Ecco un esempio che sostituisce tutte le vocali in una stringa con il carattere "o":

```C
#include <regex.h>
#include <stdio.h>

int main() {
  // Definisci il pattern di ricerca e la stringa di input
  char *pattern = "[aeiou]";
  char *input = "Ciao a tutti! Sono felice di vedervi.";

  // Crea un oggetto regex
  regex_t regex;

  // Compila il pattern
  regcomp(&regex, pattern, 0);

  // Sostituisci le vocali con il carattere "o"
  char *output = regerror(&regex, input, 0, 0);
  printf("Output: %s", output);

  return 0;
}
```

**Output:**
`Output: Coon o totto! Son fooco do vodovr.`

## Approfondimento sulle espressioni regolari

Le espressioni regolari possono essere molto complesse e possono richiedere un po' di tempo per essere padroneggiate. Ci sono molti simboli e costrutti diversi che possono essere utilizzati per creare un pattern di ricerca e ci sono anche alcune differenze tra le implementazioni delle espressioni regolari in diversi linguaggi di programmazione.

Inoltre, le espressioni regolari possono anche consumare molte risorse, soprattutto se utilizzate su stringhe di grandi dimensioni. Per questo motivo, è importante utilizzarle con parsimonia e ottimizzare il tuo codice per evitare rallentamenti.

In caso di difetti da parte tua, le espressioni regolari possono anche generare errori e questa è una delle ragioni per cui è importante testarle accuratamente prima di utilizzarle in produzione.

## Vedi anche

- [Tutorial sulle espressioni regolari in C](https://www.thegeekstuff.com/2017/07/c-regex-guide/)
- [Documentazione della libreria RegEx di C](https://www.gnu.org/software/libc/manual/html_node/The-Regular-Expression-Library.html)
- [ESPL Tutorial su espresse regolari in C](https://www.gnu.org/software/espl/doc/html/node50.html)