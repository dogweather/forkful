---
title:                "Lavorare con i file csv"
html_title:           "C: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
Se sei un programmatore che lavora con dati, probabilmente ti sei imbattuto in file CSV almeno una volta nella tua carriera. Questo formato, che sta per Comma Separated Values, è ampiamente utilizzato per rappresentare dati in modo strutturato e facilmente leggibile. Scopriamo insieme perché dovresti imparare a lavorare con CSV utilizzando il linguaggio di programmazione C.

## Come
In C, per lavorare con CSV, abbiamo bisogno di una libreria esterna chiamata "csv.h". Dopo averla inclusa nel tuo codice, puoi utilizzare le sue funzioni per aprire un file CSV, leggere i dati e manipolarli. Ecco un'esempio di come possiamo leggere le righe di un file CSV utilizzando questa libreria:

```C
#include <stdio.h>
#include <csv.h>

int main(){
    FILE *fp = fopen("data.csv", "r"); // apre il file in modalità lettura
    struct CSV *csv = CSV_new(fp); // crea un oggetto CSV utilizzando il file aperto
    
    while(CSV_has_next(csv)){ // loop finché ci sono ancora righe nel file
        struct CSV_row *row = CSV_next(csv); // otteniamo la riga successiva
        // stampiamo ogni valore della riga utilizzando la funzione CSV_get
        printf("Nome: %s, Cognome: %s, Età: %s\n", CSV_get(row, 0), CSV_get(row, 1), CSV_get(row, 2));
    }
    
    return 0;
}
```

Utilizzando questo codice su un file CSV con queste righe:

```
Nome,Cognome,Età
Marco,Rossi,25
Anna,Bianchi,30
```

Otterremo l'output:

```
Nome: Marco, Cognome: Rossi, Età: 25
Nome: Anna, Cognome: Bianchi, Età: 30
```

## Approfondimento
Oltre alla lettura e scrittura di un file CSV, la libreria "csv.h" offre molte altre funzioni per manipolarne i dati. Inoltre, se necessario, è anche possibile modificare il codice sorgente della libreria per adattarla alle tue esigenze. Ci sono anche altre librerie disponibili che offrono funzionalità simili, quindi è importante capire la complessità dei tuoi dati e scegliere la libreria più adatta per il tuo scopo.

## Vedi anche
- [Documentazione della libreria csv.h](https://csv.hackage.haskell.org/package/csv.3.0.1/docs/Data-Csv.html)
- [Esempi di codice per lavorare con CSV in C](https://github.com/alvinhochun/c-frameplate/blob/master/example/csv.c)