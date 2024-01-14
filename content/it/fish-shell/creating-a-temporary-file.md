---
title:    "Fish Shell: Creare un file temporaneo"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può sembrare una semplice operazione, ma ha molteplici utilizzi. Ad esempio, può essere utile quando si desidera salvare temporaneamente dati o output di un programma, senza dover creare un file permanente. Inoltre, i file temporanei vengono automaticamente eliminati quando il programma termina, riducendo il disordine nel sistema.

## Come fare

Creare un file temporaneo in Fish Shell è molto semplice. Basta seguire questi passaggi:

1. Utilizzare il comando `mktemp` seguito da un nome di file opzionale. Se non si specifica un nome, verrà generato uno nome casuale.
    ```
    Fish Shell - mktemp template.tmp
    ```

2. Utilizzare `touch` per creare effettivamente il file.
    ```
    Fish Shell - touch template.tmp
    ```

3. Utilizzare `echo` per inserire dati nel file.
    ```
    Fish Shell - echo "Questo è un file temporaneo" >> template.tmp
    ```

4. Ecco il contenuto del file:
    ```
    Fish Shell - cat template.tmp
    
    Questo è un file temporaneo
    ```

## Approfondimento

Creare un file temporaneo richiede un po' più di lavoro nei sistemi operativi basati su UNIX, come macOS o Linux. Questo perché questi sistemi rappresentano i file come nodi nel filesystem e l'eliminazione di un file comporta anche la rimozione del nodo. Tuttavia, nel caso dei file temporanei, non si vuole eliminare il nodo, ma solo il file che rappresenta.

Per creare un file temporaneo su questi sistemi, il sistema operativo crea un nome casuale per il file e lo aggiunge a un elenco di file che verranno eliminati all'uscita del programma. Quando il file viene creato, il sistema operativo aggiunge il nome del file al percorso in cui si trova il file, facendo sì che il file sembri esistere, anche se in realtà non è ancora stato creato.

È importante notare che i file temporanei vengono eliminati solo quando il programma termina. Se un programma si blocca o viene interrotto, il file temporaneo potrebbe rimanere nel sistema.

## Vedi anche

- [Documentazione di Fish Shell](https://fishshell.com/docs/current/)
- [Mktemp (man page)](https://linux.die.net/man/1/mktemp)
- [Touch (man page)](https://linux.die.net/man/1/touch)