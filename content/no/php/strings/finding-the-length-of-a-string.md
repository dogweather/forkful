---
date: 2024-01-20 17:47:47.154812-07:00
description: "Strenglengde handler om \xE5 telle antall tegn i en tekst. Vi gj\xF8\
  r dette for \xE5 validere inndata, bearbeide tekst, og for \xE5 limitere lengden\
  \ p\xE5 en output."
lastmod: '2024-03-13T22:44:40.876783-06:00'
model: gpt-4-1106-preview
summary: "Strenglengde handler om \xE5 telle antall tegn i en tekst. Vi gj\xF8r dette\
  \ for \xE5 validere inndata, bearbeide tekst, og for \xE5 limitere lengden p\xE5\
  \ en output."
title: "Finn lengden p\xE5 en streng"
---

{{< edit_this_page >}}

## What & Why?
Strenglengde handler om å telle antall tegn i en tekst. Vi gjør dette for å validere inndata, bearbeide tekst, og for å limitere lengden på en output.

## How to:
I PHP bruker vi `strlen` til å finne lengden av en streng. La oss prøve noen eksempler:

```PHP
<?php
$tekst = "Hei, Norge!";
echo strlen($tekst); // Skriver ut 11
?>

<?php
$greeting = "God dag";
echo strlen($greeting); // Skriver ut 7
?>
```
Legg merke til at `strlen` teller alle tegn, inkludert mellomrom.

## Deep Dive
Før i tiden, da PHP-karaktersett stort sett var begrenset til ASCII, var `strlen` enkel og grei. Nå, med UTF-8 og multibyte-karaktersett, er det ikke alltid så rett frem.

Alternativt kan du bruke `mb_strlen` for å gjøre jobben riktig med multibyte-karaktersett:

```PHP
<?php
$tekst = "Fårikål";
echo mb_strlen($tekst, 'UTF-8'); // Skriver ut 7
?>
```
Denne funksjonen er en del av Multibyte String Extension og er mer pålitelig for strenger i ulike språk og formater.

Når vi ser på implementasjonsdetaljer, bør du være oppmerksom på ytelsen. `strlen` er raskere enn `mb_strlen`, så bruk `strlen` hvis du vet at strengen ikke inneholder multibyte-tegn.

## See Also
- Offisiell PHP dokumentasjon for `strlen()`: https://www.php.net/manual/en/function.strlen.php
- Offisiell PHP dokumentasjon for `mb_strlen()`: https://www.php.net/manual/en/function.mb-strlen.php
- PHP.net Multibyte String dokumentasjon: https://www.php.net/manual/en/book.mbstring.php
