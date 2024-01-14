---
title:                "Bash: Lettura degli argomenti della linea di comando"
simple_title:         "Lettura degli argomenti della linea di comando"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

La lettura degli argomenti della riga di comando può sembrare una competenza tecnica avanzata, ma è in realtà un'abilità fondamentale per i programmatori di Bash. Conoscere come leggere e gestire gli argomenti della riga di comando può semplificare notevolmente lo sviluppo di script e permettere di creare programmi più flessibili e dinamici.

## Come Fare

Per leggere gli argomenti della riga di comando in Bash, è necessario utilizzare la variabile speciale "$1" (e successive "$2", "$3", ecc.) per accedere ai singoli argomenti. Ecco un esempio di codice che stampa il primo argomento passato al programma:

```Bash
#!/bin/bash
echo "Il primo argomento è $1"
```
Esempio di output per il comando `./script.sh Hello`:

```
Il primo argomento è Hello
```

E se volessimo stampare tutti gli argomenti passati al programma? Possiamo farlo utilizzando il costrutto `"$@"` che rappresenta tutti gli argomenti passati alla riga di comando. Vediamo un esempio:

```Bash
#!/bin/bash
echo "Gli argomenti passati sono: $@"
```

Esempio di output per il comando `./script.sh Hello World`:

```
Gli argomenti passati sono: Hello World
```

Ora che sappiamo come accedere agli argomenti della riga di comando, possiamo utilizzarli nel nostro script per creare programmi più versatili e personalizzabili.

## Approfondimento

Oltre a leggere gli argomenti della riga di comando, è anche possibile accedere alle opzioni passate tramite il comando `getopts`. Questo comando permette di definire le opzioni accettabili per il nostro programma e di accedere ai relativi valori. Ecco un esempio:

```Bash
#!/bin/bash
while getopts ":tu:" opzione; do
  case $opzione in
    t) echo "Hai passato l'opzione -t" ;;
    u) echo "Hai passato l'opzione -u con il valore: $OPTARG" ;;
    \?) echo "Opzione non riconosciuta: -$OPTARG" >&2
  esac
done
```

Esempio di output per il comando `./script.sh -t -u Name`:

```
Hai passato l'opzione -t
Hai passato l'opzione -u con il valore: Name
```

Conoscere le opzioni disponibili e sapere come utilizzarle correttamente può rendere il nostro programma ancora più efficiente e utile.

## Vedi Anche

Per ulteriori informazioni su come utilizzare gli argomenti della riga di comando in Bash, consigliamo la lettura dei seguenti link:

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameters.html)
- [Tutorial su Bash Scripting su Linuxize](https://linuxize.com/post/bash-scripting-tutorial/)
- [Guida su come utilizzare il comando getopts](https://www.computerhope.com/unix/bash/getopts.htm)

Con queste informazioni, sarai pronto ad utilizzare in modo efficace gli argomenti della riga di comando nei tuoi programmi Bash. Buon coding!