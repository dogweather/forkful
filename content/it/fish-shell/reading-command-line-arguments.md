---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Fish Shell: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa e Perche?
Leggere argomenti dalla riga di comando è il processo di accettare input da parte dell'utente, tipicamente quando viene eseguito un programma da terminale. I programmatori utilizzano questo per fornire flessibilità e personalizzazione ai loro programmi, consentendo agli utenti di specificare dati o opzioni specifiche al momento dell'esecuzione.

## Come Fare:
Di seguito sono riportati alcuni esempi di codice in Fish Shell per leggere e utilizzare argomenti dalla riga di comando:

```
#!/usr/bin/fish

# leggi e salva l'argomento in una variabile
set arg $argv[1]

# stampa l'argomento
echo "Il tuo argomento è $arg"

# verifica se è stato fornito un argomento aggiuntivo
if test "count $argv" -gt 1
	echo "Hai fornito più di un argomento!"
end
```

Esempio di output:
```
$ myscript.fish hello
Il tuo argomento è hello
```

## Approfondimento:
La lettura degli argomenti dalla riga di comando è stata introdotta nei primi sistemi operativi, come UNIX e MS-DOS, per consentire agli utenti di personalizzare i comandi che volevano eseguire. Il Fish Shell è una delle alternative al bash, un interprete più comune per la linea di comando. L'implementazione di base per leggere gli argomenti utilizza il programma `getopt`, che consente di specificare opzioni e argomenti richiesti. 

## Vedi Anche:
Per ulteriori informazioni sulla lettura degli argomenti dalla riga di comando, consulta queste risorse utili:
- Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/cmds/set.html
- Documentazione di `getopt`: https://www.gnu.org/software/libc/manual/html_node/Getopt.html