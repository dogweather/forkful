---
title:                "Conversione di una stringa in minuscolo"
aliases:
- /it/php/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:52.067135-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabeticianche quelli accentati, nella loro forma minuscola. I programmatori lo fanno per uniformare i dati inseriti dagli utenti, per comparare stringhe in modo non case-sensitive, o semplicemente per adeguarsi alle regole di stile di un testo.

## How to:
Utilizza `strtolower()` per convertire una stringa intera in minuscolo:

```PHP
<?php
$testo = "Ciao Mondo!";
$testoMinuscolo = strtolower($testo);
echo $testoMinuscolo; // ciao mondo!
?>
```

Per gestire correttamente anche le stringhe con caratteri multibyte, come lettere accentate o alfabeti non latini, usa `mb_strtolower()`:

```PHP
<?php
$testo = "Ciao Móndo!";
$testoMinuscolo = mb_strtolower($testo);
echo $testoMinuscolo; // ciao móndo!
?>
```
Ricorda: imposta il charset corretto con `mb_internal_encoding('UTF-8');` prima di utilizzare le funzioni `mb_`.

## Deep Dive
La funzione `strtolower()` esiste in PHP da tempo immemore, ed è il modo più semplice e diretto per convertire una stringa in minuscolo. Tuttavia, questa funzione non considera i caratteri al di fuori dell'ASCII standard, come le lettere accentate tipiche della lingua italiana.

Per questa ragione, è nata `mb_strtolower()` della famiglia di funzioni Multibyte String, introdotte per lavorare con encoding multi-byte come UTF-8. Queste funzioni sono essenziali in un contesto multilingua e globalizzato.

Il comportamento di `mb_strtolower()` è simile a `strtolower()`, ma con il supporto per un'ampia gamma di caratteri. La funzione conta sul parametro 'encoding', che se non specificato, prende il valore di default dalla configurazione interna, che può essere impostata con `mb_internal_encoding()`.

Alternativamente, esistono altri modi per manipolare la case delle stringhe. Le funzioni `ucfirst()`, `lcfirst()`, `ucwords()`, e `mb_convert_case()` per esempio, offrono diverse opzioni per modificare maiuscole e minuscole in situazioni specifiche.

## See Also
- Documentazione ufficiale PHP `strtolower`: https://www.php.net/manual/it/function.strtolower.php
- Documentazione ufficiale PHP `mb_strtolower`: https://www.php.net/manual/it/function.mb-strtolower.php
- Documentazione ufficiale PHP sulle stringhe multibyte: https://www.php.net/manual/it/ref.mbstring.php
- Tutorial sulle funzioni di gestione stringhe in PHP: https://www.w3schools.com/php/php_ref_string.asp
