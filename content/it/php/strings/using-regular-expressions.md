---
title:                "Utilizzo delle espressioni regolari"
aliases:
- /it/php/using-regular-expressions.md
date:                  2024-02-03T19:17:45.475627-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo delle espressioni regolari"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

Le espressioni regolari (regex) in PHP sono modelli usati per corrispondere combinazioni di caratteri nelle stringhe, consentendo operazioni sofisticate di ricerca-e-sostituzione e validazione di dati. I programmatori sfruttano le regex per la loro potenza e flessibilità nell'analisi del testo, nella validazione di form o nello scraping di dati web, rendendole uno strumento indispensabile nell'arsenale di uno sviluppatore.

## Come fare:

PHP supporta le espressioni regolari attraverso la libreria PCRE (Perl Compatible Regular Expressions), offrendo un ricco set di funzioni. Ecco come usarle:

### Corrispondenza di un modello:

Per verificare se un modello esiste all'interno di una stringa, si usa `preg_match()`. Questa funzione restituisce 1 se il modello è stato trovato nella stringa e 0 se non lo è.

```php
if (preg_match("/\bweb\b/i", "PHP è un linguaggio di scripting web")) {
    echo "È stata trovata una corrispondenza.";
} else {
    echo "Non è stata trovata alcuna corrispondenza.";
}
// Output: È stata trovata una corrispondenza.
```

### Trovare tutte le corrispondenze:

`preg_match_all()` viene usato quando è necessario trovare tutte le occorrenze di un modello all'interno di una stringa.

```php
$text = "gatti e cani";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Output: Array ( [0] => gatti [1] => e [2] => cani )
```

### Sostituire il testo:

Per sostituire il testo che corrisponde a un'espressione regolare, si usa `preg_replace()`. È incredibilmente potente per formattare e pulire i dati.

```php
$testoOriginale = "15 Aprile, 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$sostituzione = '${1}1,$3';
echo preg_replace($pattern, $sostituzione, $testoOriginale);
// Output: Aprile1,2003
```

### Suddividere le stringhe:

Puoi suddividere una stringa in un array usando `preg_split()`, specificando un modello per il delimitatore.

```php
$text = "PHP è, estremamente popolare, linguaggio di scripting";
$parti = preg_split("/,\s*/", $text);
print_r($parti);
// Output: Array ( [0] => PHP è [1] => estremamente popolare [2] => linguaggio di scripting )
```

Inoltre, per modelli e compiti regex complessi, framework e librerie come il componente `Finder` di Symfony o la collezione di funzioni helper di Laravel potrebbero fornire uno strato di astrazione più conveniente. Tuttavia, comprendere e utilizzare le funzioni PCRE integrate in PHP è cruciale per un'elaborazione e una validazione del testo efficienti direttamente all'interno degli script PHP.
