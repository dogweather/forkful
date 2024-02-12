---
title:                "Ricerca e sostituzione del testo"
date:                  2024-02-03T18:08:13.873478-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ricerca e sostituzione del testo"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

La ricerca e la sostituzione di testo in C prevedono l'identificazione di specifiche sottostringhe all'interno di una stringa più grande e la loro sostituzione con altre sottostringhe. I programmatori eseguono queste operazioni per manipolare dati di testo - per compiti che vanno dalla sanificazione e formattazione dei dati alla generazione dinamica di contenuti.

## Come fare:

C non dispone di funzioni incorporate per eseguire direttamente la ricerca e la sostituzione sulle stringhe. Tuttavia, è possibile ottenere questo combinando varie funzioni di gestione delle stringhe disponibili nella libreria `<string.h>` insieme ad alcune logiche personalizzate. Di seguito è riportato un esempio base su come cercare una sottostringa all'interno di una stringa e sostituirla. Per semplicità, questo esempio assume una dimensione del buffer sufficiente e non gestisce problemi di allocazione della memoria, che dovresti considerare nel codice di produzione.

```c
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void replaceSubstring(char *source, char *sub, char *new_sub) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t len_sub = strlen(sub), len_new_sub = strlen(new_sub);
    size_t len_up_to_match;

    while ((tmp = strstr(tmp, sub))) {
        // Calcola la lunghezza fino alla corrispondenza
        len_up_to_match = tmp - source;
        
        // Copia la parte prima della corrispondenza
        memcpy(insert_point, source, len_up_to_match);
        insert_point += len_up_to_match;
        
        // Copia la nuova sottostringa
        memcpy(insert_point, new_sub, len_new_sub);
        insert_point += len_new_sub;
        
        // Sposta oltre la corrispondenza nella stringa sorgente
        tmp += len_sub;
        source = tmp;
    }
    
    // Copia qualsiasi parte rimanente della stringa sorgente
    strcpy(insert_point, source);
    
    // Stampa la stringa modificata
    printf("Stringa modificata: %s\n", buffer);
}

int main() {
    char sourceStr[] = "Ciao, questa è una prova. Questa prova è semplice.";
    char sub[] = "prova";
    char newSub[] = "esempio";
    
    replaceSubstring(sourceStr, sub, newSub);
    
    return 0;
}
```

Output di esempio:
```
Stringa modificata: Ciao, questa è un esempio. Questo esempio è semplice.
```

Questo codice dimostra un approccio semplice per cercare tutte le istanze di una sottostringa (`sub`) in una stringa sorgente e sostituirle con un'altra sottostringa (`newSub`), utilizzando la funzione `strstr` per trovare il punto di inizio di ogni corrispondenza. È un esempio molto basilare che non gestisce scenari complessi come sottostringhe sovrapposte.

## Approfondimento

L'approccio utilizzato nella sezione "Come fare" è fondamentale, illustrando come realizzare la ricerca e sostituzione di testo in C senza librerie di terze parti. Storicamente, a causa dell'enfasi di C sulla gestione della memoria a basso livello e sulle prestazioni, la sua libreria standard non incapsula funzionalità di manipolazione delle stringhe ad alto livello come quelle trovate in linguaggi come Python o JavaScript. I programmatori devono gestire manualmente la memoria e combinare varie operazioni sulle stringhe per ottenere i risultati desiderati, il che aumenta la complessità ma offre più controllo ed efficienza.

È fondamentale notare che questo approccio manuale può essere soggetto ad errori, in particolare quando si gestiscono allocazioni di memoria e dimensioni dei buffer. Una gestione errata può portare a buffer overflow e corruzione della memoria, rendendo il codice vulnerabile a rischi per la sicurezza.

In molti scenari pratici, specialmente quelli che richiedono un elaborazione del testo complessa, spesso vale la pena considerare l'integrazione di librerie di terze parti come PCRE (Perl Compatible Regular Expressions) per la ricerca e sostituzione basata su regex, che può semplificare il codice e ridurre il potenziale per errori. Inoltre, gli standard e i compilatori C moderni offrono sempre più funzioni incorporate e alternative più sicure per la manipolazione delle stringhe, mirando a mitigare le insidie comuni osservate nei codici C più vecchi. Tuttavia, la comprensione fondamentale del trattamento manuale dei testi rimane una competenza preziosa nel kit di strumenti di un programmatore, specialmente per ottimizzare le applicazioni critiche per le prestazioni.
