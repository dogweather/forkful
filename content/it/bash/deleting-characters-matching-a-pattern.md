---
title:                "Bash: Eliminare caratteri corrispondenti a un modello"
simple_title:         "Eliminare caratteri corrispondenti a un modello"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler cancellare dei caratteri che corrispondono ad uno specifico modello in un programma Bash. Può essere utile quando si vuole pulire un'input di testo o quando si vuole ottenere solo alcune parti di un file di log.

## Come fare
Per cancellare dei caratteri corrispondenti ad un modello, è possibile utilizzare il comando `sed` in combinazione con l'opzione `s` per la sostituzione e `g` per la globalità. Ad esempio, se si vuole cancellare tutte le vocali da una stringa, il codice potrebbe essere il seguente:

```
Bash
stringa="Ciao a tutti!"
output=$(echo "$stringa" | sed 's/[aeiou]//g')
echo "$output"
```

L'output di questo codice sarebbe "C ttt!" poiché tutte le vocali sono state cancellate dalla stringa originale. È importante notare che il comando `sed` modifica solo l'output e non cambia la stringa originale.

## Analisi Approfondita
Il comando `sed` può essere utilizzato in combinazione con espressioni regolari per individuare modelli specifici da cancellare. Ad esempio, se si vuole eliminare tutte le parole che iniziano con la lettera "a" da un file di testo, il codice potrebbe essere il seguente:

```
Bash
sed -i '/\ba\S*/d' file.txt
```

Questo comando utilizza l'opzione `-i` per modificare direttamente il file di testo originale e l'espressione regolare `/\ba\S*/d` per individuare e cancellare le parole che iniziano con "a".

## Vedi anche
- [Documentazione Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial Introduttivo su Sed](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)
- [Espressioni Regolari in Bash](https://www.regular-expressions.info/tutorial.html)