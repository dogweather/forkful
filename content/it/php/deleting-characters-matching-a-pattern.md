---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Rimozione dei caratteri corrispondenti a un modello in PHP

## Cos'è e perché?

La rimozione di caratteri corrispondenti a un modello in PHP è un'operazione che permette di eliminare da una stringa tutti i caratteri che soddisfano un certo criterio. Questa tecnica è utile quando hai bisogno di pulire o manipolare dati.

## Come fare:

Ecco un esempio di come potresti rimuovere tutti i caratteri numerici da una stringa:

```PHP
<?php
$stringa = "1Ciao, 2Sono 3Marco!";
$stringa_pulita = preg_replace("/[0-9]/", "", $stringa);
echo $stringa_pulita;
?>
```

Questo produrrà:

```
Ciao, Sono Marco!
```

In questo esempio, il modello è `[0-9]`, che corrisponde a tutti i numeri. La funzione `preg_replace()` rimpiazza tutte le occorrenze di questo modello con una stringa vuota, effettivamente rimuovendo i numeri.

## Approfondimento

Lo standard PHP usa l'espressione regolare di compatibilità Perl, che è una potente tool per il pattern matching. Tuttavia, c'erano alternative come `ereg()`, che utilizza le espressioni regolari POSIX, ma questa funzione è deprecata in PHP 5.3.0.

Le espressioni regolari permettono di specificare modelli molto più complessi rispetto a singoli caratteri. Ad esempio, il modello `/[0-9]{3}/` corrisponde a una sequenza di tre numeri.

A volte, un'alternativa a `preg_replace()` può essere la funzione `str_replace()`. Questa funzione non accetta modelli, ma la sua semplicità la rende adatta se devi semplicemente sostituire un sottoinsieme fisso di caratteri.

## Leggi anche

Per saperne di più sulla rimozione di caratteri corrispondenti a un modello in PHP, controlla i seguenti link:

1. Documentazione PHP su `preg_replace()`: https://www.php.net/manual/en/function.preg-replace.php
2. Tutorial dettagliato sulle espressioni regolari in PHP: https://www.phpro.org/tutorials/Regular-Expressions.html
3. Modelli e regex in PHP: https://www.php.net/manual/en/book.pcre.php
4. Sostituisci modelli con `str_replace()`: https://www.php.net/manual/en/function.str-replace.php