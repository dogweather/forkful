---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Fish Shell: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Controllare se una directory esiste è semplicemente una procedura che richiede al computer di verificare se c'è una cartella con un determinato nome in una specifica posizione nel sistema. I programmatori spesso fanno questo per assicurarsi che il loro codice funzioni correttamente, evitando errori e problemi futuri.

## Come fare:

```
Fish Shell...
if test -d <directory_name>
echo "La directory esiste!"
end

if not test -d <directory_name>
echo "La directory non esiste!"
end
```
Output:
```
La directory esiste!
```

```
Fish Shell...
set directory_check (test -d <directory_name>; echo)
echo $directory_check
```
Output:
```
1
```

```
Fish Shell...
test -d <directory_name>
echo $status
```
Output:
```
0
```

## Approfondimento:

Controllare se una directory esiste è un'operazione importante nella programmazione, poiché consente di gestire meglio le risorse del sistema. Se una directory non esiste, il programma potrebbe creare automaticamente una nuova directory per contenere i file o gestire ulteriori funzionalità.

Esistono diversi modi per verificare se una directory esiste in diversi linguaggi di programmazione, come ad esempio l'uso di comandi terminali come `ls` o `dir` in Linux o Windows. Tuttavia, Fish Shell semplifica notevolmente il processo con la funzione `test -d`, che restituisce un risultato diretto in base alla presenza o meno della directory.

## Vedi anche:

- [5 Linux/Unix commands to Check if a Directory Exists](https://www.tecmint.com/check-if-directory-exists-in-linux/)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Alternative to Test Command in Fish Shell](https://www.cyberciti.biz/faq/how-to-use-test-command-in-fish-shell/)