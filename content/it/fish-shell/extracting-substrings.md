---
title:                "Estrazione di sottostringhe"
html_title:           "Fish Shell: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'estrazione di sottostringhe è il processo di selezione di una parte specifica di una stringa più grande, in base a determinate regole o criteri. I programmatori spesso lo fanno per manipolare e gestire meglio i dati all'interno di un programma o uno script.

## Come fare:
Per estrarre sottostringhe in Fish Shell, puoi utilizzare il comando `string sub`. Ad esempio, per estrarre i primi cinque caratteri da una stringa, puoi usare il seguente comando:
```
string sub -l 5 "questa è una stringa"
```
Questo restituirà `questa`.

Puoi anche specificare un punto di inizio e uno di fine, utilizzando la sintassi `string sub INIZIO FINE`. Ad esempio, se volessi estrarre i caratteri dalla posizione 3 alla 7, puoi scrivere:
```
string sub 3 7 "questa è una stringa"
```
Questo restituirà `sta è`.

## Approfondimento:
L'estrazione di sottostringhe è un concetto comune nei linguaggi di programmazione e ha una lunga storia. In Fish Shell, puoi anche utilizzare la sintassi `substring` per estrarre sottostringhe. Alcune alternative per estrarre sottostringhe includono l'utilizzo delle funzioni `cut` o `grep` in combinazione con espressioni regolari.

Per quanto riguarda l'implementazione, Fish Shell utilizza la funzione `substr` della libreria string.h per eseguire l'estrazione delle sottostringhe.

## Vedi anche:
- [Documentazione ufficiale di Fish Shell su string sub](https://fishshell.com/docs/current/cmds/string.html#sub)
- [Altre informazioni su espressioni regolari in Fish Shell](https://fishshell.com/docs/current/index.html#regular-expressions)