---
title:                "Fish Shell: Estrazione di sottostringhe"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
In questo articolo, impareremo come e perché estrarre sottostringhe utilizzando il Fish Shell. Estrarre sottostringhe può essere utile per manipolare e ottenere informazioni specifiche da una stringa più grande.

## Come fare
Per estrarre una sottostringa, utilizziamo il comando `string` seguito dal punto esclamativo `!` seguito dalla posizione di inizio e fine della sottostringa desiderata. Ad esempio:

```
Fish Shell 1.0.0 (Iwashi)
$ set stringa "Questo è un'importante stringa di esempio!"
$ echo $stringa
Questo è un'importante stringa di esempio!
$ set sottostringa $stringa[10..20]
$ echo $sottostringa
importante
```

Nell'esempio sopra, abbiamo creato una variabile `stringa` con una stringa di esempio e poi estrapolato la sottostringa di caratteri che vanno dal decimo al ventesimo posto utilizzando il comando `set` e la variabile `$stringa`.

In alternativa, possiamo utilizzare i comandi di sostituzione delle stringhe per estrarre una sottostringa da una variabile già esistente. Ad esempio, possiamo utilizzare il comando `strmatch` per trovare una corrispondenza e estrarre la sottostringa che precede o segue la corrispondenza. Vediamo un esempio:

```
Fish Shell 1.0.0 (Iwashi)
$ set url "https://it.wikipedia.org/wiki/Shell"
$ echo $url
https://it.wikipedia.org/wiki/Shell
$ set suburl strmatch -r "it.wikipedia.org/wiki/(.*)/" -- $url
$ echo $suburl
Shell
```

In questo caso, abbiamo utilizzato il comando `strmatch` per cercare l'espressione regolare `it.wikipedia.org/wiki/(.*)/` nella variabile `url` e quindi estrarre la sottostringa che segue immediatamente la corrispondenza, in questo caso "Shell".

## Approfondimento
Il Fish Shell supporta anche altre funzionalità per l'estrazione di sottostringhe, come utilizzare la posizione di un carattere specifico come punto di inizio o fine della sottostringa. Possiamo anche utilizzare l'indice negativo per indicare la posizione di una sottostringa in base agli ultimi caratteri della stringa. Ad esempio:

```
Fish Shell 1.0.0 (Iwashi)
$ set stringa "4, 8, 15, 16, 23, 42"
$ echo $stringa
4, 8, 15, 16, 23, 42
$ set numero $stringa[4..$]
$ echo $numero
16, 23, 42
```
In questo esempio, abbiamo assegnato alla variabile `stringa` una serie di numeri separati da virgola. Poi, utilizzando l'indice negativo -4, abbiamo estrapolato una sottostringa a partire dal quarto carattere dall'ultima virgola fino alla fine della stringa.

## Vedi anche
- [Documentazione ufficiale di Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutorial di Fish Shell su dev.to](https://dev.to/sreyajkumar/fish-shell-tutorial-3kno)
- [Estrarre sottostringhe con awk su Linuxize](https://linuxize.com/post/how-to-extract-substrings-in-bash/)