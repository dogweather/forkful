---
title:                "Estrazione di sottostringhe"
html_title:           "Bash: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se stai imparando a programmare in Bash, una delle cose che vorrai imparare è come estrarre sottostringhe da una stringa più grande. Questa abilità può essere utile in molte situazioni, come nella manipolazione dei dati o nella ricerca di informazioni specifiche.

## Come

Per estrarre una sottostringa da una stringa in Bash, puoi utilizzare il comando `cut`. Ad esempio, se vogliamo estrarre i primi 5 caratteri da una stringa, possiamo utilizzare il seguente codice:

```Bash
stringa="Questo è un esempio"
echo "${stringa:0:5}"
```

L'output sarà "Quest".

Puoi anche specificare l'indice di inizio e il numero di caratteri da estrarre:

```Bash
echo "${stringa:4:6}"
```

L'output sarà "o è un".

## Deep Dive

Esistono diverse opzioni e formati per estrarre sottostringhe in Bash, come specificare l'indice di inizio e fine, utilizzare espressioni regolari e altro ancora. Puoi trovare maggiori informazioni e approfondimenti sulla documentazione ufficiale di Bash o su siti web di tutorial.

## Vedi anche

- [Documentazione ufficiale di Bash](https://www.gnu.org/software/bash/)
- [Tutorial su estrazione di sottostringhe in Bash](https://linuxize.com/post/bash-extract-substring/)
- [Ulteriori esempi di estrazione di sottostringhe](https://www.tecmint.com/extract-portion-of-string-in-bash/)