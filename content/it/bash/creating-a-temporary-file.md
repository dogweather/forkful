---
title:    "Bash: Creazione di un file temporaneo"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

In questo articolo parleremo di come creare e utilizzare un file temporaneo in Bash. Creare un file temporaneo può essere utile in molte situazioni, come ad esempio quando si deve gestire un grande volume di dati o quando si vuole mantenere un file temporaneo per un breve periodo di tempo prima di cancellarlo.

## Come Fare

Per creare un file temporaneo in Bash, possiamo utilizzare il comando `mktemp`. Questo comando creerà automaticamente un file temporaneo con un nome univoco, ideale per evitare conflitti di nome con altri file o per garantire una maggiore sicurezza.

```Bash
#!/bin/bash
temp_file=$(mktemp)

echo "Questo è un file temporaneo" > $temp_file
cat $temp_file

# Output: Questo è un file temporaneo
```

Una volta creato il file temporaneo, possiamo utilizzarlo per eseguire diverse operazioni, come ad esempio la lettura o la scrittura di dati. Dopo aver completato le operazioni, è importante rimuovere il file temporaneo utilizzando il comando `rm` per evitare di occupare spazio inutilmente.

```Bash
#!/bin/bash
temp_file=$(mktemp)

echo "Questo è un file temporaneo" > $temp_file
cat $temp_file

# Eseguire altre operazioni sul file temporaneo

rm $temp_file
```

## Approfondimento

Il comando `mktemp` permette di impostare diverse opzioni per personalizzare il file temporaneo creato. Ad esempio, possiamo specificare il percorso in cui il file deve essere creato utilizzando l'opzione `-p` o possiamo impostare un prefisso per il nome del file utilizzando l'opzione `-t`.

```Bash
#!/bin/bash
temp_file=$(mktemp -p /percorso/desiderato -t prefisso-)

echo "Questo è un file temporaneo" > $temp_file
cat $temp_file

# Output: Questo è un file temporaneo

rm $temp_file
```

Inoltre, possiamo anche utilizzare il comando `mktemp` per creare una directory temporanea, semplicemente specificando l'opzione `-d`. Questo può essere utile, ad esempio, quando si vogliono estrarre dei file temporanei all'interno di una cartella per semplificare la loro gestione.

## Vedi Anche

- [Documentazione ufficiale di `mktemp`](https://man7.org/linux/man-pages/man3/mktemp.3.html)
- [Altri comandi utili in Bash](https://www.shellscript.sh/other1.html)
- [Una guida completa a Bash scripting](https://ryanstutorials.net/bash-scripting-tutorial/)