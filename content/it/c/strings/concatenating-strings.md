---
title:                "Concatenazione di stringhe"
date:                  2024-02-03T17:53:52.858591-07:00
model:                 gpt-4-0125-preview
simple_title:         "Concatenazione di stringhe"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/concatenating-strings.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

La concatenazione di stringhe in C consiste nell'unire due o più stringhe da capo a coda per formare una nuova stringa. I programmatori eseguono questa operazione per costruire dinamicamente stringhe a runtime, essenziale per creare messaggi significativi, percorsi di file o qualsiasi dato assemblato da varie fonti di stringhe.

## Come fare:

In C, le stringhe sono array di caratteri che terminano con un carattere nullo (`\0`). A differenza dei linguaggi di livello superiore, C non fornisce una funzione di concatenazione di stringhe incorporata. Invece, si utilizzano le funzioni `strcat()` o `strncat()` dalla libreria `<string.h>`.

Ecco un semplice esempio utilizzando `strcat()`:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destinazione[50] = "Ciao, ";
    char sorgente[] = "Mondo!";

    strcat(destinazione, sorgente);

    printf("%s\n", destinazione);  // Output: Ciao, Mondo!
    return 0;
}
```

La funzione `strcat()` prende due argomenti: la stringa di destinazione (che deve avere abbastanza spazio per contenere il risultato concatenato) e la stringa sorgente. Quindi, aggiunge la stringa sorgente alla stringa di destinazione.

Per avere più controllo sul numero di caratteri concatenati, `strncat()` è più sicura da usare:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destinazione[50] = "Ciao, ";
    char sorgente[] = "Mondo!";
    int num = 3; // Numero di caratteri da aggiungere

    strncat(destinazione, sorgente, num);

    printf("%s\n", destinazione);  // Output: Ciao, Mon
    return 0;
}
```

Questo limita la concatenazione ai primi `num` caratteri della stringa sorgente, aiutando a prevenire overflow del buffer.

## Approfondimento

Le funzioni `strcat()` e `strncat()` fanno parte della libreria standard di C fin dalla sua nascita, riflettendo la natura a basso livello del linguaggio che richiede la gestione manuale delle stringhe e della memoria. A differenza di molti linguaggi di programmazione moderni che trattano le stringhe come oggetti di prima classe con operatori di concatenazione incorporati (come `+` o `.concat()`), l'approccio del C richiede una comprensione più approfondita dei puntatori, dell'allocazione della memoria e dei potenziali pericoli come gli overflow del buffer.

Sebbene `strcat()` e `strncat()` siano ampiamente utilizzati, sono spesso criticati per il loro potenziale di creare vulnerabilità di sicurezza se non usati con attenzione. Gli overflow del buffer, dove i dati superano la memoria allocata, possono portare a crash o essere sfruttati per l'esecuzione arbitraria di codice. Di conseguenza, i programmatori stanno sempre più orientandosi verso alternative più sicure, come `snprintf()`, che offre un comportamento più prevedibile limitando il numero di caratteri scritti nella stringa di destinazione in base alla sua dimensione:

```c
char destinazione[50] = "Ciao, ";
char sorgente[] = "Mondo!";
snprintf(destinazione + strlen(destinazione), sizeof(destinazione) - strlen(destinazione), "%s", sorgente);
```

Questo metodo è più prolisso ma significativamente più sicuro, evidenziando un cambiamento nelle pratiche di programmazione C verso la priorità a sicurezza e robustezza rispetto alla brevità.

Nonostante queste sfide, la concatenazione di stringhe in C è una competenza fondamentale, cruciale per una programmazione efficace nel linguaggio. Comprendere le sue sfumature e i rischi associati è chiave per padroneggiare la programmazione in C.
