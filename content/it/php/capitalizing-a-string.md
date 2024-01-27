---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Cosa e Perché?
Capitalizzare una stringa significa convertire la prima lettera di ogni parola in maiuscolo. I programmatori lo fanno per standardizzare l'output dei testi, come titoli o nomi propri, rendendoli più leggibili e formalmente corretti.

## How to: - Come fare:
```PHP
<?php
$frase = "benvenuti al corso di PHP!";
$frase_capitalizzata = ucwords($frase);

echo $frase_capitalizzata; // Output: Benvenuti Al Corso Di PHP!
?>
```
In alternativa, per capitalizzare solo la prima lettera della stringa:
```PHP
<?php
$titolo = "ciao mondo!";
$titolo_capitalizzato = ucfirst($titolo);

echo $titolo_capitalizzato; // Output: Ciao mondo!
?>
```

## Deep Dive - Approfondimento
Una volta in PHP, per capitalizzare stringhe, eravamo limitati a `ucfirst()` e `strtoupper()`. La prima capitalizza solo la prima lettera della stringa, la seconda converte tutto in maiuscolo. Poi è arrivata `ucwords()` che capitalizza la prima lettera di ogni parola, perfetta per titoli e nomi.

Ma occhio ai caratteri speciali! PHP non capisce sempre i limiti delle parole se ci sono caratteri come trattini o apostrofi. Per casi complessi, potremmo dover usare espressioni regolari.

Come alternativa possiamo usare `mb_convert_case()` quando lavoriamo con Unicode, utile per lingue con caratteri speciali.

## See Also - Vedi Anche
- La documentazione ufficiale di PHP su `ucwords()`: https://www.php.net/manual/en/function.ucwords.php
- La documentazione ufficiale di PHP su `mb_convert_case()`: https://www.php.net/manual/en/function.mb-convert-case.php
- Tutorial su espressioni regolari in PHP: https://www.php.net/manual/en/reference.pcre.pattern.syntax.php
