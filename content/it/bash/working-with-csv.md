---
title:                "Bash: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
In questo articolo parleremo di come lavorare con file CSV utilizzando il linguaggio di programmazione Bash. I dati in formato CSV sono molto comuni e possono essere facilmente gestiti tramite lo scripting Bash. Imparare a lavorare con questi file ti sarà utile in molte situazioni, quindi è un ottimo modo per ampliare le tue abilità di programmazione.

## Come Fare
Per iniziare a lavorare con i file CSV in Bash, dovrai utilizzare alcuni comandi e operazioni specifici. Qui di seguito troverai alcuni esempi di codice con spiegazioni su come utilizzarli.

### Lettura di un file CSV
Per leggere i dati da un file CSV in Bash, puoi utilizzare il comando `read` in un ciclo `while`, come mostrato nell'esempio seguente:

```Bash
while read line; do
    echo $line # stampa ogni riga del file
    # inserisci qui le tue operazioni sui dati
done < file.csv
```

In questo esempio, il file CSV viene aperto e i dati vengono letti riga per riga. Puoi quindi utilizzare la variabile `$line` per accedere ai valori dei singoli campi della riga, a seconda del separatore utilizzato nel file CSV (solitamente una virgola).

### Scrittura in un file CSV
Per scrivere i dati in un file CSV, puoi utilizzare il comando `printf` in un ciclo `for`, come mostrato nell'esempio seguente:

```Bash
echo "Nome,Cognome,Età" > output.csv # stampa l'intestazione del file
for ((i=1; i<=10; i++)); do
    printf "Persona %d,Cognome %d,%d\n" $i $i $((RANDOM % 60 + 18)) >> output.csv  # scrive i dati nel file
done
```

In questo esempio, viene creato un nuovo file CSV chiamato "output.csv". L'intestazione viene prima stampata utilizzando il comando `echo`, quindi il ciclo `for` genera dei dati casuali e li scrive nel file utilizzando il comando `printf`.

### Modifica di un file CSV
Per modificare un file CSV esistente, puoi utilizzare il comando `sed`, come mostrato nell'esempio seguente:

```Bash
sed -i 's/A,B,C/D,E,F/g' file.csv # sostituisce i valori A, B e C con D, E e F rispettivamente
```

In questo esempio, il comando `sed` sostituisce i valori specificati nel file CSV con il nuovo valore, utilizzando l'opzione `-i` per modificare il file originale.

## Approfondimento
Esistono molte altre operazioni che puoi eseguire sui file CSV utilizzando lo scripting Bash. Ad esempio, puoi utilizzare i comandi `grep` e `awk` per cercare e manipolare i dati in base a determinati criteri. Puoi anche utilizzare le variabili e le espressioni aritmetiche per eseguire calcoli sui dati.

Inoltre, è possibile scrivere script più complessi che combinano diverse operazioni sui dati CSV, rendendo il processo di gestione dei file molto più efficiente e automatizzato.

## Vedi Anche
- [Comandi e operazioni base di Bash](https://www.linux.com/tutorials/bash-101-tutorials-series/)
- [Documentazione ufficiale del linguaggio Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Come lavorare con i dati in Bash](https://opensource.com/article/18/5/how-work-data-bash)