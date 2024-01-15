---
title:                "Lavorare con i file csv"
html_title:           "TypeScript: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con dati tabellari, come quelli presenti in Excel o Google Sheets, è probabile che ti trovi spesso ad utilizzare file CSV. Questo formato è molto popolare perché è facile da leggere dai programmi, anche per quelli scritti in TypeScript. In questo articolo, esploreremo come lavorare con i file CSV utilizzando TypeScript.

## Come fare

Per iniziare, assicurati di avere TypeScript installato sul tuo computer. Utilizza il comando `npm install -g typescript` per installare l'ultima versione. Dopodiché, crea un nuovo file TypeScript con l'estensione `.ts`.

```TypeScript
import fs from 'fs';
import csv from 'csv-parse';

fs.readFile('file.csv', 'utf8', (err, csvString) => {
    if (err) {
        console.log(err);
    } else {
        csv(csvString, {}, (err, output) => {
            if (err) {
                console.log(err);
            } else {
                // output contiene i dati del CSV in formato array
                console.log(output);
            }
        });
    }
});
```

In questo esempio, abbiamo utilizzato il modulo `fs` per leggere il file CSV come una stringa, poi abbiamo passato la stringa al modulo `csv` che converte i dati in un array di array utilizzando la virgola come delimitatore. Da qui in poi, puoi utilizzare gli array per manipolare i dati a tuo piacimento.

## Deep Dive

Esaminiamo più nel dettaglio la sintassi utilizzata nell'esempio precedente. 

Per prima cosa, abbiamo importato i moduli `fs` e `csv-parse` utilizzando la parola chiave `import`. Questo ci permette di utilizzare le funzioni e le variabili all'interno di questi moduli all'interno del nostro codice.

Poi abbiamo utilizzato il metodo `readFile` del modulo `fs` per leggere il file CSV come una stringa. Il primo parametro è il path del file, il secondo è la codifica della stringa e il terzo è una funzione di callback che viene eseguita una volta che il file è stato letto. La funzione di callback prende come parametro un eventuale errore e la stringa letta dal file.

La stringa viene poi passata al modulo `csv` utilizzando il metodo `csv`. Il secondo parametro è un oggetto vuoto, perché nel nostro caso non abbiamo bisogno di specificare alcuna opzione. Il terzo parametro è ancora una funzione di callback che prende come parametri eventuali errori e il risultato della conversione in formato array.

A questo punto, possiamo utilizzare l'array per eseguire operazioni come il filtraggio dei dati o l'aggiunta di nuove colonne.

## See Also

Ecco alcuni utili link per saperne di più su come lavorare con i file CSV in TypeScript:

- Documentazione ufficiale di TypeScript: https://www.typescriptlang.org/docs/
- Documentazione di Node.js: https://nodejs.org/en/docs/
- Documentazione del modulo csv-parse: https://csv.js.org/parse/
- Tutorial su TypeScript e i file CSV: https://ultimatecourses.com/blog/working-with-csv-files-in-typescript

Buon lavoro con i file CSV in TypeScript!