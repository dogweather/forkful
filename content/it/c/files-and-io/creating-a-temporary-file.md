---
title:                "Creazione di un file temporaneo"
aliases: - /it/c/creating-a-temporary-file.md
date:                  2024-02-03T17:55:08.867665-07:00
model:                 gpt-4-0125-preview
simple_title:         "Creazione di un file temporaneo"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è e perché?
Creare un file temporaneo in C comporta la generazione di un file destinato a essere utilizzato per una breve durata, solitamente come spazio temporaneo per l'elaborazione o la memorizzazione dei dati. I programmatori lo fanno per gestire i dati temporanei senza influire sulla memoria permanente del programma o per assicurarsi che i dati sensibili vengano cancellati dopo l'uso.

## Come fare:
Creare un file temporaneo nel linguaggio di programmazione C può sfruttare funzioni come `tmpfile()` e `mkstemp()`.

**Usare `tmpfile()`**: Questa funzione crea un file temporaneo unico che viene automaticamente cancellato quando il programma termina o il file viene chiuso.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Impossibile creare file temporaneo");
        return 1;
    }

    // Scrivere dati nel file temporaneo
    fputs("Questo è un test.\n", temp);

    // Riavvolgere e leggere ciò che abbiamo scritto
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Cancellato automaticamente alla chiusura o all'uscita del programma
    fclose(temp);

    return 0;
}
```
**Output dell'esempio:**
```
Questo è un test.
```

**Usare `mkstemp()`**: Offre maggior controllo sulla posizione del file temporaneo e sui suoi permessi. Richiede una stringa modello che termina con `XXXXXX` che poi viene sostituita con una sequenza unica per prevenire collisioni di nomi.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/miotemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("Impossibile creare file temporaneo");
        return 1;
    }
    
    printf("File temporaneo creato: %s\n", template);

    // I file temporanei creati con mkstemp() devono essere cancellati manualmente
    unlink(template);

    close(fd);
    return 0;
}
```
**Output dell'esempio:**
```
File temporaneo creato: /tmp/miotemp-abc123
```

## Approfondimenti
Il concetto di file temporanei non è unico del C, ma è una funzionalità comune in molti ambienti di programmazione a causa della sua utilità nel gestire dati effimeri. La funzione `tmpfile()`, standardizzata nello standard ISO C, crea un file con un nome unico in una directory standard, ma la sua esistenza è fugace, rendendola ideale per operazioni sicure o temporanee.

Una limitazione notevole di `tmpfile()` è la sua dipendenza dalla directory temporanea predefinita, che potrebbe non essere adeguata per tutte le applicazioni, specialmente in termini di permessi o sicurezza. Al contrario, `mkstemp()` consente di specificare la directory e assicura una creazione del file sicura con nomi di file univoci garantiti modificando la stringa modello fornita, offrendo una soluzione più versatile a scapito della gestione manuale dei file.

Tuttavia, la creazione di file temporanei può introdurre vulnerabilità di sicurezza, come le condizioni di gara, se non gestita correttamente. Ad esempio, `tmpfile()` e `mkstemp()` affrontano diversi aspetti della creazione sicura di file temporanei (cancellazione automatica e generazione sicura dei nomi, rispettivamente), ma nessuna delle due è una panacea. Gli sviluppatori devono considerare le specificità delle esigenze di sicurezza della loro applicazione, inclusi i potenziali rischi introdotti dai file temporanei, e potrebbero dover implementare ulteriori salvaguardie oltre a quanto offerto da queste funzioni.

Nel panorama più ampio della programmazione, alternative come lo storage in memoria (ad esempio, utilizzando strutture dati dinamiche o file mappati in memoria) potrebbero offrire prestazioni o sicurezza migliori per la gestione dei dati temporanei. Tuttavia, i file temporanei fisici rimangono uno strumento cruciale in molti scenari, specialmente per set di dati grandi o quando è coinvolta la comunicazione tra processi.
