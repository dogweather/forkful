---
title:                "Capitalizzare una stringa"
html_title:           "PHP: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## COS'È E PERCHÉ?

Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in una maiuscola. Questo è utile per i programmatori quando vogliono migliorare la presentabilità e la leggibilità dei testi generati dal loro codice.

## COME SI FA:

Ecco un semplice esempio di come capitalizzare una stringa in PHP:

```php
<?php
$stringa = "ciao mondo";
$stringaCapitalizzata = ucwords($stringa);
echo $stringaCapitalizzata;
?>
```
Output del codice sarà:

```
Ciao Mondo
```
In questo esempio, la funzione `ucwords()` di PHP viene utilizzata per capitalizzare ogni parola di una stringa.

## APPROFONDIMENTI

1) **Contesto storico**: La funzione `ucwords()` è disponibile in PHP sin dalla versione 4.0.0, rilasciata nel 2000.

2) **Alternative**: Oltre all'uso di `ucwords()`, si può utilizzare la funzione `mb_convert_case()` se si lavora con multibyte string (stringhe che contengono caratteri non-latini o speciali).

Esempio:

```php
<?php
$stringa = "ciao mondo";
$stringaCapitalizzata = mb_convert_case($stringa, MB_CASE_TITLE, "UTF-8");
echo $stringaCapitalizzata;
?>
```
Output del codice sarà:

```
Ciao Mondo
```
3) **Dettagli Implementativi**: `ucwords()` converte solo il primo carattere di ogni parola in maiuscolo, mentre `mb_convert_case()` è più potente perché supporta i caratteri Unicode. Tuttavia, l'uso di quest'ultima funzione può comportare un leggero impatto sulle prestazioni rispetto alla prima.

## APPROFONDIMENTI

Manuali online di PHP per le funzioni [`ucwords()`](http://php.net/manual/en/function.ucwords.php) e [`mb_convert_case()`](http://php.net/manual/en/function.mb-convert-case.php). Consigliamo vivamente di consultare questi link per capire meglio come queste funzioni funzionano e per scoprire tutte le altre funzioni di manipolazione delle stringhe disponibili in PHP.