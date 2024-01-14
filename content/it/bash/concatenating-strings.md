---
title:                "Bash: Unire stringhe."
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché concatenare le stringhe in Bash

La concatenazione di stringhe è un'operazione comune nei linguaggi di programmazione, ed è particolarmente utile in Bash quando si lavora con testi o variabili. Unificare più stringhe può semplificare il codice e aggiungere flessibilità ai comandi. Vediamo come fare.

## Come concatenare le stringhe in Bash

Per concatenare due stringhe in Bash, è necessario utilizzare il comando `printf` seguito dal simbolo di percentuale `%s` per ogni stringa che si vuole concatenare. Ad esempio, se si vuole unire le stringhe "Ciao" e "mondo", il codice sarebbe il seguente:

```
Bash
helloworld=$(printf "%s%s" "Ciao" "mondo")
echo $helloworld
```
Output: `Ciao mondo`

Se si desidera aggiungere uno spazio tra le due stringhe, si può utilizzare il carattere di spazio dopo il primo `%s` nel comando `printf`, come mostrato di seguito:

```
Bash
helloworld=$(printf "%s %s" "Ciao" "mondo")
echo $helloworld
```
Output: `Ciao mondo`

Per concatenare più di due stringhe, basta inserire più simboli di percentuale e le relative stringhe all'interno del comando `printf`. Inoltre, è possibile utilizzare variabili al posto delle stringhe rigide:

```
Bash
greeting="Ciao"
name="mondo"
helloworld=$(printf "%s %s" $greeting $name)
echo $helloworld
```
Output: `Ciao mondo`

## Approfondimento sulla concatenazione delle stringhe

Quando si concatenano stringhe in Bash, è importante ricordare che all'interno del comando `printf` le stringhe vengono unite senza spazi aggiunti automaticamente. Ciò significa che se si desidera inserire uno spazio tra le due stringhe, è necessario specificarlo esplicitamente nel comando. Inoltre, è possibile concatenare qualsiasi tipo di dato, non solo stringhe, utilizzando il comando `printf`.

## Vedi anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)
- [Tutorial su Bash di Linux.com](https://www.linux.com/learn/introduction-bash-variables)
- [Guida completa a Bash di Tecmint](https://www.tecmint.com/best-books-to-learn-bash-shell-scripting/)