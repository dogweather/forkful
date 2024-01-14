---
title:    "Bash: Lettura dei parametri della riga di comando"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore o un appassionato di tecnologia, probabilmente hai già sentito parlare di Bash. Bash è un linguaggio di scripting utilizzato principalmente in sistemi operativi Unix e Linux. Ciò significa che Bash è uno strumento potente per automatizzare le attività quotidiane su un computer. Una delle sue funzionalità principali è la capacità di leggere e interpretare gli argomenti della riga di comando. In questo articolo esploreremo come farlo.

## Come farlo

Per leggere gli argomenti della riga di comando in Bash, puoi utilizzare la variabile `$@`, che contiene tutti gli argomenti passati al programma quando viene eseguito. Ad esempio, se il tuo programma Bash si chiama `script.sh` e lo esegui con il comando `./script.sh arg1 arg2`, la variabile `$@` conterrà i valori `arg1` e `arg2`. Puoi quindi utilizzare questi valori all'interno del tuo script come preferisci.

Un esempio di codice Bash per leggere gli argomenti della riga di comando potrebbe essere il seguente:

```Bash
#!/bin/bash

# Legge e stampa tutti gli argomenti della riga di comando
for arg in $@; do
  echo $arg
done
```

Se esegui questo script con il comando `./script.sh uno due tre`, otterrai l'output seguente:

```
uno
due
tre
```

Puoi anche specificare un argomento specifico utilizzando l'operatore `[]` e indicando la posizione desiderata. Ad esempio, se vuoi ottenere solo il secondo argomento, puoi utilizzare la sintassi `$@[2]`. Inoltre, puoi anche verificare se sono stati passati argomenti nella riga di comando utilizzando la variabile `$#`, che contiene il numero totale di argomenti.

## Approfondimento

Esistono anche altre opzioni per leggere e manipolare gli argomenti della riga di comando in Bash. Ad esempio, puoi utilizzare l'operatore `shift` per spostare gli argomenti e ottenere solo quelli che ti interessano. Inoltre, puoi utilizzare il comando `getopts` per gestire opzioni e argomenti più complessi nella riga di comando.

Inoltre, è importante tenere presente che gli argomenti della riga di comando possono contenere spazi o caratteri speciali. Per gestire questo, puoi utilizzare le virgolette doppie o singole intorno agli argomenti nella riga di comando. Ad esempio, se il tuo argomento contiene uno spazio come `arg with space`, puoi utilizzare `./script.sh "arg with space"` nella riga di comando.

## Vedi anche

Per ulteriori informazioni su come leggere e gestire gli argomenti della riga di comando in Bash, puoi consultare questi siti:

- [Bash Scripting Tutorial - Command Line Parameters](https://ryanstutorials.net/bash-scripting-tutorial/bash-parameters.php)
- [Linuxize - Understanding Bash Command Line Arguments](https://linuxize.com/post/understanding-bash-command-line-arguments/)
- [The Linux Documentation Project - Command Line Parameters](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)

Ora che conosci le basi, puoi iniziare a sfruttare al massimo la potenza degli argomenti della riga di comando in Bash per automatizzare le tue attività quotidiane. Buon coding!