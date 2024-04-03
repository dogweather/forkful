---
date: 2024-01-26 03:40:37.177260-07:00
description: "Rimuovere le virgolette da una stringa in PHP significa eliminare quei\
  \ fastidiosi caratteri di virgoletta doppia (`\"`) o singola (`'`) che possono\u2026"
lastmod: '2024-03-13T22:44:43.505190-06:00'
model: gpt-4-0125-preview
summary: Rimuovere le virgolette da una stringa in PHP significa eliminare quei fastidiosi
  caratteri di virgoletta doppia (`"`) o singola (`'`) che possono interferire con
  la logica del codice o le query del database.
title: Rimuovere le virgolette da una stringa
weight: 9
---

## Come fare:
Ecco un esempio semplice utilizzando le funzioni integrate di PHP:

```php
$quotedString = "'Ciao,' disse, \"È una bella giornata!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Stampa: Ciao, disse, È una bella giornata!
```

Semplice, giusto? Questa funzione `str_replace()` prende un array di caratteri da rimuovere dalla stringa, includendo sia le virgolette singole che quelle doppie.

## Approfondimento
Nei primi giorni di PHP, gli sviluppatori dovevano essere particolarmente cauti con le virgolette nelle stringhe, soprattutto quando inserivano dati in un database. Le virgolette gestite in modo non corretto potevano portare ad attacchi di SQL injection. È nata quindi la funzionalità delle magic quotes, che auto-escapava i dati di input. È stata deprecata ed infine rimossa perché incoraggiava pratiche di codifica errate e problemi di sicurezza.

Ora, usiamo funzioni come `str_replace()` o regex con `preg_replace()` per pattern più avanzati. Ecco un esempio con regex:

```php
$quotedString = "'Ciao,' disse, \"È una bella giornata!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Per i dati JSON, si potrebbe usare `json_encode()` con opzioni come `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` per evitare barre inverse extra nelle tue virgolette.

Quando implementate, considerate i casi limite. Cosa succede se la vostra stringa deve avere certe virgolette, come i dialoghi in una storia o le misure in pollici? Il contesto conta, quindi adattate la rimozione delle virgolette all'uso previsto dei dati.

## Vedi Anche
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: Prevenzione SQL Injection](https://owasp.org/www-community/attacks/SQL_Injection)
