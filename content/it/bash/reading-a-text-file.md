---
title:    "Bash: Lettura di un file di testo"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è una delle attività fondamentali della programmazione Bash. Essa consente di manipolare i dati presenti in un file in modo rapido ed efficiente, rendendo il processo di elaborazione dei dati automatizzato e semplificato. Continuate a leggere per scoprire come leggere un file di testo utilizzando Bash.

## Come Fare

Leggere un file di testo in Bash è un'operazione semplice e il codice seguente mostra uno dei modi per farlo:

```Bash
#!/bin/bash

file="mio_file.txt"

while read line; do
    echo $line
done < $file
```

In questo esempio, la variabile "file" contiene il nome del file di testo che si desidera leggere. Il ciclo "while" legge il file riga per riga e utilizza il comando "echo" per stampare ogni riga nel terminale. Infine, il simbolo "<" viene utilizzato per indicare al ciclo "while" di utilizzare il file come input.

L'output di questo script sarà ogni riga del file di testo stampata nel terminale. Se si desidera specificare una riga o un'area specifica del file da leggere, si può utilizzare il comando "head" o "tail" per ottenere solo le prime o le ultime righe del file, rispettivamente.

## Approfondimento

Per una maggiore flessibilità e controllo, è possibile utilizzare il comando "read" per accedere ai dati specifici all'interno di un file di testo. Ad esempio, se il file di testo ha una formattazione di tipo CSV (comma-separated values), è possibile estrarre facilmente i dati dalla prima colonna utilizzando il comando "cut" insieme ad un ciclo "while", come mostrato di seguito:

```Bash
#!/bin/bash

file="mio_file.csv"

while read line; do
    echo $line | cut -d, -f1
done < $file
```

In questo esempio, il file di testo "mio_file.csv" viene letto e ogni riga viene passata al comando "cut", che imposta il delimitatore come virgola (dalla flag "-d") e specifica che si desidera ottenere solo il primo campo (dalla flag "-f1"). L'output sarà ogni elemento della prima colonna del file di testo stampato nel terminale.

## Vedi Anche

- [Bash Shell Scripting: Leggere un file di testo](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
- [Bash Shell Scripting: Cicli](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_01.html)
- [The Unix School: Bash Scripting Tutorial](http://www.theunixschool.com/p/the-unix-school.html)