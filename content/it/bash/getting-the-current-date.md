---
title:    "Bash: Ottenere la data corrente"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Perché

Se hai mai programmato in Bash, potresti esserti trovato nella situazione di dover ottenere la data corrente all'interno del tuo script. Questo può essere utile per stampare la data di un file o per avere un record delle esecuzioni del tuo script. In questa guida, impareremo come ottenere la data corrente utilizzando il linguaggio di scripting Bash.

## Come Fare

Per ottenere la data corrente in Bash, puoi utilizzare il comando `date`. Questo comando restituisce la data e l'ora attuali nel formato specificato dall'utente.

```
Bash
date "+%d-%m-%Y %H:%M:%S"
```

Output:

```
19-05-2021 10:57:32
```

Puoi anche utilizzare le opzioni del comando `date` per ottenere la data in formati diversi. Ad esempio, per ottenere solo il giorno della settimana abbreviato, puoi utilizzare:`

```
Bash
date "+%a"
```

Output:

```
Wed
```

Puoi anche utilizzare il comando `date` per ottenere la data di un file specificato utilizzando il parametro `-r` seguito dal percorso del file.

```
Bash
date -r sample_file.txt
```

Output:

```
Wed 19 May 2021 10:57:32 AM UTC
```

## Deep Dive

Il comando `date` utilizza la variabile di ambiente `TZ` per determinare il fuso orario corrente. Se non viene specificata alcuna variabile `TZ`, il comando utilizzerà il fuso orario del sistema.

Puoi anche utilizzare il comando `date` per eseguire calcoli sulla data corrente. Ad esempio, se vuoi ottenere la data del giorno successivo, puoi utilizzare il comando:

```
Bash
date -d "tomorrow"
```

Output:

```
Thu May 20 10:57:32 UTC 2021
```

Per ulteriori opzioni e informazioni sul comando `date`, puoi fare riferimento al manuale utilizzando il comando `man date`.

## Vedi Anche

* [Documentazione di Date su Linux](https://linux.die.net/man/1/date)
* [Tutorial su Bash Scripting](https://linuxize.com/post/bash-scripting-tutorial/)