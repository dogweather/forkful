---
title:                "Convertire una stringa in minuscolo"
html_title:           "Fish Shell: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché convertire una stringa in minuscolo con Fish Shell?

Se stai utilizzando Fish Shell, potresti aver bisogno di convertire una stringa in minuscolo per vari motivi, come manipolare i dati di un file o confrontare stringhe in modo case-insensitive. In questo articolo vedremo come farlo utilizzando Fish Shell.

## Come farlo con Fish Shell

La sintassi per convertire una stringa in minuscolo con Fish Shell è semplice:

```
stringa | string tolower
```

In questo modo, il valore della stringa verrà convertito in minuscolo e restituito come output.

Ecco un esempio pratico:

```
echo "Hello World" | string tolower  # output: hello world
```

In questo caso, abbiamo utilizzato `echo` per generare una stringa di testo e il comando `string tolower` per convertire la stringa in minuscolo.

È anche possibile utilizzare questa sintassi all'interno di uno script Fish, in questo modo:

```
set nome "Marco"
set nome_lowercase $nome | string tolower 
echo $nome_lowercase  # output: marco
```

In questo esempio, abbiamo salvato la stringa "Marco" nella variabile `nome` e poi utilizzato `string tolower` per convertirla in minuscolo e salvarla nella variabile `nome_lowercase`.

## Deep Dive

La conversione di una stringa in minuscolo con Fish Shell è possibile grazie all'utilizzo del comando `string tolower`. Una possibile implementazione di questo comando è la seguente:

```
function string tolower -d "Converts a string to lowercase"
  set -l result
  for char in (string by -s $argv[1])
    set char (string tolower $char)
    set result $result$char
  end
  echo $result
end
```

In questo comando, utilizziamo un ciclo `for` per andare attraverso ogni carattere della stringa e utilizzare il comando `string tolower` per convertire ogni carattere in minuscolo. Infine, utilizziamo `echo` per restituire la stringa convertita nel risultato.

## Vedi anche

- Documentazione ufficiale di Fish Shell: https://fishshell.com/docs/current/
- Converting Strings in Fish Shell: https://www.linux.com/topic/desktop/how-convert-strings-fish-shell/
- Funzioni Fish Shell avanzate: https://hacksoflife.blogspot.com/2016/03/fish-functions-file-namesubstr.html