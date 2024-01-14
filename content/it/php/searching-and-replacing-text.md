---
title:                "PHP: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

C'è un'attività comune che ogni programmatore di PHP deve affrontare: la ricerca e la sostituzione di testo. Questa semplice operazione può risparmiare tonnellate di tempo e sforzi nella modifica dei file di codice, ed è uno strumento essenziale da conoscere per ogni sviluppatore.

## Come

Ci sono diverse funzioni e metodi in PHP che consentono di cercare e sostituire testo all'interno di una stringa o di un file. Ecco alcune delle più comuni:

```PHP
// Funzione str_replace
$stringa = "Questo è un esempio";
$stringa_modificata = str_replace("esempio", "cambio", $stringa);
echo $stringa_modificata; // Output: Questo è un cambio

// Funzione preg_replace
$stringa = "12345";
$stringa_modificata = preg_replace("/\d/", "*", $stringa);
echo $stringa_modificata; // Output: *****

// Metodo strtr
$stringa = "Ciao mondo!";
$stringa_modificata = strtr($stringa, "io", "11");
echo $stringa_modificata; // Output: Ca11 m11nd11!
```

Questi sono solo alcuni esempi di come è possibile utilizzare le funzioni PHP per cercare e sostituire testo. Si consiglia di consultare la documentazione ufficiale di PHP per una lista completa di funzioni e metodi disponibili.

## Deep Dive

Se si vuole approfondire ulteriormente la ricerca e la sostituzione di testo in PHP, ci sono alcuni aspetti importanti da tenere a mente:

- Le funzioni PHP per la ricerca e la sostituzione di testo sono sensibili alle maiuscole e minuscole. Ciò significa che una stringa come "Esempio" non sarà sostituita da "esempio" utilizzando le funzioni descritte sopra.
- È possibile utilizzare espressioni regolari con la funzione preg_replace per una maggiore flessibilità nella ricerca e sostituzione di testo.
- Le funzioni e i metodi di ricerca e sostituzione di testo in PHP possono essere combinati con altre funzioni come explode() e implode() per manipolare facilmente i dati all'interno di una stringa o di un file.

Inoltre, è importante sapere che la maggior parte delle funzioni PHP per la ricerca e la sostituzione di testo non sono case-sensitive, il che significa che una stringa di ricerca come "esempio" sarà trovata sia in "Esempio" che in "esempio". Tuttavia, esistono alcune funzioni che consentono di impostare la case-sensitivty come str_replace() e str_ireplace().

## Vedere anche

- [Documentazione ufficiale di PHP su str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [Documentazione ufficiale di PHP su strtr()](https://www.php.net/manual/en/function.strtr.php)
- [Documentazione ufficiale di PHP su preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Guida alle espressioni regolari in PHP](https://www.php.net/manual/en/regexp.reference.php)