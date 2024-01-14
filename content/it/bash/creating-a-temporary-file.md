---
title:    "Bash: Creare un file temporaneo"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può essere utile in molte situazioni, ad esempio quando si vuole eseguire una determinata operazione su un file senza alterarlo o per creare uno spazio di lavoro temporaneo per lo sviluppo di uno script.

## Come Fare

Per creare un file temporaneo in Bash, è possibile utilizzare il comando `mktemp` seguito da una maschera di nome file, in questo modo:

```Bash
tempfile=$(mktemp)
```

Il file temporaneo verrà creato nella directory corrente e il suo nome sarà assegnato alla variabile `tempfile`.

Per specificare una specifica directory per la creazione del file, è possibile utilizzare l'opzione `-p`. Ad esempio:

```Bash
tempfile=$(mktemp -p /path/to/directory)
```

Una volta creato il file temporaneo, è possibile utilizzarlo nel codice Bash, ad esempio per scrivere dei dati al suo interno o per eseguire delle operazioni su di esso.

```Bash
echo "Questo è un esempio di testo" > $tempfile
```

Inoltre, è importante eliminare il file temporaneo una volta che non serve più, utilizzando il comando `rm` seguito dal nome del file. Si consiglia di farlo all'interno di uno script Bash utilizzando `trap` per gestire eventuali errori e assicurarsi che il file venga sempre eliminato.

```Bash
trap "rm $tempfile" EXIT
```

## Approfondimento

Creare un file temporaneo con il comando `mktemp` garantisce che il file venga creato in modo sicuro e univoco, evitando eventuali conflitti con file esistenti. Inoltre, il comando gestisce anche la pulizia dei file temporanei in caso di interruzioni o errori durante l'esecuzione dello script.

Inoltre, è possibile specificare una maschera di nome file personalizzata utilizzando l'opzione `-t` e specificare dei permessi di accesso con l'opzione `-m`.

```Bash
tempfile=$(mktemp -t customfilename -m 700)
```

Utilizzando la maschera di nome file personalizzata, è possibile avere un maggior controllo sul nome del file temporaneo creato, mentre specificando i permessi di accesso si può garantire una maggiore sicurezza nel caso in cui il file contenga dati sensibili.

## Vedi Anche

Per ulteriori informazioni su come utilizzare i file temporanei in Bash, si consiglia di consultare i seguenti link:

- [https://linuxize.com/post/bash-create-temporary-file/](https://linuxize.com/post/bash-create-temporary-file/)
- [https://www.howtogeek.com/67469/htg-explains-what-are-the-linux-filesystems-ext2-ext3-ext4/](https://www.howtogeek.com/67469/htg-explains-what-are-the-linux-filesystems-ext2-ext3-ext4/)
- [https://man7.org/linux/man-pages/man1/mktemp.1.html](https://man7.org/linux/man-pages/man1/mktemp.1.html)