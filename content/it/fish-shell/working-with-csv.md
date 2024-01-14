---
title:                "Fish Shell: Lavorare con i csv"
simple_title:         "Lavorare con i csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Chiunque lavori con dati strutturati come i file CSV può trarre vantaggio dall'utilizzo del Fish Shell per automatizzare e semplificare il processo di manipolazione dei dati.

## Come fare

Utilizzare il Fish Shell per lavorare con file CSV è un processo semplice e intuitivo. Di seguito sono riportati alcuni esempi di codice e relativi output che illustreranno come questo shell può essere utile.

```fish
# Visualizza il contenuto di un file CSV
fish -c "cat file.csv"

# Ordina i dati per colonna
fish -c "sort -k {colonna} file.csv"

# Rimuove i duplicati
fish -c "uniq file.csv"

# Filtra i dati per valore
fish -c "grep {valore} file.csv"

# Modifica il valore di una cella
fish -c "sed -i 's/{valore_originale}/{nuovo_valore}/g' nome_file.csv"

# Calcola la somma della colonna
fish -c "awk '{sum+=$colonna} END {print sum}' file.csv"
```

## Approfondimento

Il Fish Shell offre numerosi strumenti utili per lavorare con file CSV. Ad esempio, la funzione `grep` può essere utilizzata in combinazione con espressioni regolari per filtrare i dati in base a criteri specifici. Inoltre, è possibile utilizzare il comando `awk` per eseguire operazioni matematiche sui dati, come nel caso dell'esempio sopra riportato.

## Vedi anche

Ecco alcuni link utili per approfondire l'argomento:

- [Documentazione ufficiale del Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial su come lavorare con file CSV utilizzando il Fish Shell](https://medium.com/@escline/lavorare-con-file-csv-utilizzando-il-fish-2a506cc964f3)
- [Guida completa al Fish Shell](https://www.freecodecamp.org/news/the-coolest-features-of-the-fish-shell-4-09eedfd15b0b/)