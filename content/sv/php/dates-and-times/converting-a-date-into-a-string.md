---
date: 2024-01-20 17:37:08.333479-07:00
description: "How to: PHP anv\xE4nder `date()`-funktionen f\xF6r att omvandla datum\
  \ till textstr\xE4ngar. H\xE4r \xE4r exempel p\xE5 hur du anv\xE4nder den."
lastmod: '2024-03-13T22:44:38.007621-06:00'
model: gpt-4-1106-preview
summary: "PHP anv\xE4nder `date()`-funktionen f\xF6r att omvandla datum till textstr\xE4\
  ngar."
title: "Omvandla ett datum till en str\xE4ng"
weight: 28
---

## How to:
PHP använder `date()`-funktionen för att omvandla datum till textsträngar. Här är exempel på hur du använder den:

```PHP
<?php
$datum = new DateTime("now", new DateTimeZone("Europe/Stockholm"));
echo $datum->format('Y-m-d H:i:s');
?>
```
Ovanstående kod skriver ut aktuellt datum och tid som en sträng, exempelvis `2023-04-01 15:23:47`.

## Deep Dive
I PHP:s barndom, användes `date()` tillsammans med `strtotime()` för att hantera datum. Idag är `DateTime` klassen modernare, med bättre stöd för tidszoner och objektorienterad programmering. Alternativ innefattar `IntlDateFormatter` för lokalanpassning eller DateTimeImmutable för oföränderliga objekt. Detaljerna kring hur `DateTime::format` fungerar inkluderar formateringsparametrar som definierar utdatans utseende – `Y` för år, `m` för månad och `d` för dag, etc.

## See Also
Dokumentation om PHP:s datum- och tidsfunktioner: [php.net/manual/en/book.datetime.php](https://www.php.net/manual/en/book.datetime.php)
PHP DateTime klassreferens: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
IntlDateFormatter för lokalanpassade datum: [php.net/manual/en/class.intldateformatter.php](https://www.php.net/manual/en/class.intldateformatter.php)
