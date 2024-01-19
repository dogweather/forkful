---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att omvandla ett datum till en sträng (eller "string" på engelska) innebär att vi ändrar formatet på datumdata till en läsbar textform. Programmerare gör detta för att förbättra läsbarheten och för att det är lättare att manipulera och använda strängar i olika sammanhang.

## Hur man gör:

PHP erbjuder en inbyggd funktion `date()` för att omvandla datum till strängar. Här är ett grundläggande exempel:

```PHP 
<?php 
$datum = date("Y-m-d"); // år-månad-dag
echo $datum;
?>
```

När du kör ovanstående kod får du utskriften av dagens datum som en sträng i format åååå-mm-dd (t.ex. 2022-01-19).

## Fördjupning

Historiskt sett har datumhantering alltid varit en utmaning i programmering på grund av variationer i datumformat och tidszoner. 

Även om PHP:s `date()` funktion är lättillgänglig, finns det alternativ om du vill ha mer flexibilitet. En av dessa är `DateTime` klassen, vilket tillåter dig att använda objekt för att hantera datum och tider.

Om du använder `date()`, kommer PHP att använda serverns tidszon som standard. För att ställa in specifika tidszoner kan du använda `date_default_timezone_set()` funktionen.

```PHP
<?php
date_default_timezone_set('Europe/Stockholm');
$datum = date("Y-m-d H:i:s"); 
echo $datum;
?>
```

Denna kod ger oss dagens datum och aktuell tid i Stockholm som en sträng.

## Se också

För ytterligare läsning och fördjupning, besök följande länkar:

- PHP:s officiella dokumentation om datumfunktioner: [PHP: Date/Time Functions - Manual](https://www.php.net/manual/en/book.datetime.php)

- Dessutom rekommenderar vi W3Schools informativa guide: [PHP Date and Time Functions](https://www.w3schools.com/php/php_date.asp)

- På PHP.net hittar du även utförligt om `DateTime` klassen: [PHP: The DateTime Class - Manual](https://www.php.net/manual/en/class.datetime.php)

Observera att du kan hitta prisvärda och kvalitativa kurser på exempelvis Udemy: [PHP Date and Time Course](https://www.udemy.com/topic/php/)