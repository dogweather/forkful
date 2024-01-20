---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng innebär att konvertera en strängrepresentation av ett datum till ett format som datorprogram kan förstå och manipulera. Programmerare gör detta för att hantera och manipulera data mer effektivt, speciellt när datan används i beräkningar eller logikstyrning.

## Så här gör du:

PHP erbjuder inbyggda funktioner för att tolka datum från strängar. En av de mest använda funktionerna är `strtotime()` som konverterar en datumsträng till ett Unix-timestamp.

```PHP
<?php
$dateString = "20-May-2022";
$timestamp = strtotime($dateString);
echo $timestamp;
?>
```
Exempelutmatningen skulle vara ett Unix-timestamp, som: `1653628800`.

Men om du vill ha ett mer läsbart datumformat, kan du använda PHP:s `date()`-funktion.

```PHP
<?php
$dateString = "20-May-2022";
$timestamp = strtotime($dateString);
$readableDate = date('Y-m-d', $timestamp);
echo $readableDate;
?>
```
Detta skulle ge en utmatning som: `2022-05-20`.

## Djupdykning

Historiskt sett har PHP alltid erbjudit `strtotime()` funktionen, men i PHP 5.2 och framåt fick vi tillgång till `DateTime`-classen som erbjuder mer flexibilitet och funktioner.

Alternativt till `strtotime()`, kan du också använda `DateTime::createFromFormat()` för att bestämma det specifika formatet som din datumsträng har.

```PHP
<?php
$dateString = "20-May-2022";
$date = DateTime::createFromFormat('j-M-Y', $dateString);
$timestamp = $date->getTimestamp();
echo $timestamp;
?>
```

Viktigt att påpeka är att även om dessa funktioner är kraftfulla, har de sina begränsningar. De kan ha problem att tolka felaktiga eller konstiga datumformat. I sådana fall kan du behöva skriva egna tolkningsfunktioner.

## Se även:

Kolla in följande länkar för mer information om PHP:s datumhantering:

1. PHP manualen för [`strtotime()`](https://www.php.net/manual/en/function.strtotime.php)
2. PHP manualen för [`DateTime`](https://www.php.net/manual/en/class.datetime.php)
3. StackOverflow-post om [när man bör använda `DateTime` vs `strtotime()`](https://stackoverflow.com/questions/3133857/php-datetime-vs-strtotime) för datumtolkning i PHP.