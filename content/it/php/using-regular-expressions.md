---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Arduino: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Le espressioni regolari, o regex, consentono di cercare e manipolare testo con pattern dettagliati. Sono ovunque perché semplificano operazioni complesse su stringhe.

## How to:
### Ricerca di una stringa
```php
<?php
$testo = "Trova la parola magica: abracadabra.";
$pattern = "/magica/";
preg_match($pattern, $testo, $corrispondenze);
print_r($corrispondenze);
?>
```
Output:
```
Array
(
    [0] => magica
)
```

### Sostituzione di una stringa
```php
<?php
$testo = "Saluti da Roma.";
$pattern = "/Roma/";
$sostituzione = "Venezia";
echo preg_replace($pattern, $sostituzione, $testo);
?>
```
Output:
```
Saluti da Venezia.
```

### Divisione di una stringa
```php
<?php
$testo = "uno,due,tre,quattro";
$pattern = "/,/";
print_r(preg_split($pattern, $testo));
?>
```
Output:
```
Array
(
    [0] => uno
    [1] => due
    [2] => tre
    [3] => quattro
)
```

## Deep Dive
Le espressioni regolari sono nate negli anni '50, ma la versione che usiamo oggi deriva dalla notazione introdotta da Ken Thompson negli anni '60. Le alternative a regex includono l'analisi manuale delle stringhe o l'uso di funzioni di manipolazione di stringhe integrate, ma spesso sono meno potenti. L'implementazione in PHP avviene attraverso la libreria PCRE (Perl Compatible Regular Expressions), che fornisce funzioni quali `preg_match`, `preg_replace`, e `preg_split`.

## See Also
- [PHP PCRE Docs](https://www.php.net/manual/en/book.pcre.php) - Documentazione ufficiale sulle funzioni PCRE in PHP.
- [Regex101](https://regex101.com/) - Tool online per testare le espressioni regolari.
- [PHP The Right Way](https://phptherightway.com/#regular_expressions) - Guida alle best practices in PHP, sezione di regex.
