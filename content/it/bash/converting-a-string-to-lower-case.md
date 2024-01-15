---
title:                "Convertire una stringa in minuscolo"
html_title:           "Bash: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in lettere minuscole può essere utile quando si vuole standardizzare il formato dei dati o quando si vuole confrontare stringhe senza considerare le maiuscole e minuscole.

## Come fare

Per convertire una stringa in lettere minuscole in Bash, è possibile utilizzare il comando `tr`. Ad esempio, per convertire la stringa "Hello World" in lettere minuscole, è possibile utilizzare il seguente codice:

```Bash
echo "Hello World" | tr '[:upper:]' '[:lower:]'
```

L'output sarà `hello world`.

Per automatizzare questo processo e applicarlo a una variabile, è possibile utilizzare il comando `sed` in combinazione con la sintassi `$(...)`. Ad esempio:

```Bash
stringa="AbCde"
echo $(echo $stringa | sed 's/.*/\L&/')
```

L'output sarà `abcde`.

## Deep Dive

In Bash, le stringhe possono essere manipolate utilizzando vari comandi come `tr`, `sed`, `awk` e `cut`. Ognuno di questi comandi ha la sua specifica funzionalità e può essere utilizzato per eseguire operazioni diverse sulle stringhe.

Il comando `tr` viene utilizzato principalmente per convertire o eliminare caratteri all'interno di una stringa. In questo caso, stiamo utilizzando la sintassi `[` e `]` per indicare un insieme di caratteri da considerare, ad esempio `[a-z]` indica tutti i caratteri minuscoli dalla `a` alla `z`. Utilizzando la sintassi `[:upper:]` e `[:lower:]`, stiamo specificando rispettivamente tutti i caratteri maiuscoli e tutti i caratteri minuscoli.

Il comando `sed` viene utilizzato per sostituire parti di una stringa con un'altra. In questo caso, stiamo utilizzando il pattern `s/.*/\L&/`, dove `s` indica la sostituzione, `.*` indica qualsiasi carattere e `\L` indica che i caratteri successivi dovranno essere convertiti in minuscolo.

## Vedi anche

- Tutorial su comandi di manipolazione delle stringhe in Bash: https://www.shell-tips.com/bash/string-manipulation/
- Documentazione ufficiale del comando `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Documentazione ufficiale del comando `sed`: https://www.gnu.org/software/sed/manual/sed.html