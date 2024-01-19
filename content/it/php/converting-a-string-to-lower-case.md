---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La conversione di una stringa in minuscolo è una operazione comune nel campo della programmazione. Si fa per semplificare le operazioni di confronto di stringhe, eliminando le differenze tra maiuscole e minuscole.

## Ecco Come:

Utilizziamo la funzione `strtolower()` per convertire una stringa in minuscolo in PHP.

```PHP
$stringa = 'Ciao Mondo';
$stringa_minuscolo = strtolower($stringa);
echo $stringa_minuscolo; // Output: ciao mondo
```

## Approfondimenti

Ora, delve più profondamente. La funzione `strtolower()` esiste nei linguaggi di programmazione da molto tempo, risalente alle prime edizioni di Unix negli anni '70. 

C'è un'alternativa `mb_strtolower()`, che offre più controllo sulla codifica dei caratteri rispetto a `strtolower()`. 

In termini di implementazione, `strtolower()` utilizza una semplice mappa di caratteri per mappare tutti i caratteri ASCII maiuscoli ai loro equivalenti minuscoli.

```PHP
$accent_string = 'CIAO MÒNDO';
$accent_string_lower = mb_strtolower($accent_string, 'UTF-8');
echo $accent_string_lower; // Output: ciao mòndo
```

## Vedi Anche

Per ulteriori dettagli su come convertire una stringa in minuscolo in PHP:
- Documentazione PHP [strtolower()](http://php.net/manual/en/function.strtolower.php)
- Documentazione PHP [mb_strtolower()](http://php.net/manual/en/function.mb-strtolower.php)
- StackOverflow: ["When to use mb_strtolower instead of strtolower?"](https://stackoverflow.com/questions/3686425/when-to-use-mb-strtolower-instead-of-strtolower)
- Confronto tra [difference strtolower() vs mb_strtolower()](https://www.geekhideout.com/urlcode.shtml)