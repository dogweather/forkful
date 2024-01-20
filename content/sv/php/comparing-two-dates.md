---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum i programmering handlar om att avgöra om ett datum är tidigare, senare eller samma som ett annat datum. Detta görs för att hantera tidsrelaterad logik; tid och datum är avgörande inom allt från bokningssystem till vetenskaplig forskning.

## Hur Du Gör:

Här är hur du kan jämföra två datum med hjälp av PHPs inbyggda DateTime klass:

```PHP
$datum1 = new DateTime('2020-01-01');
$datum2 = new DateTime('2020-12-31');

if ($datum1 < $datum2) {
    echo "Datum1 är tidigare än Datum2.";
} else if ($datum1 > $datum2) {
    echo "Datum1 är senare än Datum2.";
} else {
    echo "Datum1 och Datum2 är samma dag.";
}
```

Om `Datum1` är 1:a januari 2020 och `Datum2` är 31:a december 2020, kommer output vara:

```
Datum1 är tidigare än Datum2.
```

## Djupdykning:

Begreppet att jämföra två datum har varit grunden för tidsbaserade beräkningar sedan urminnes tider, långt innan datorerna skapades. I PHP, en språk som ligger på server-sidan, används datumjämförelser flitigt i diverse applikationer.

Det finns alternativa sätt att jämföra datum på i PHP. En vanlig metod är att omvandla båda datumen till ett `timestamp` format med `strtotime()` funktionen, och jämföra dessa.

```PHP
$timestamp1 = strtotime('2020-01-01');
$timestamp2 = strtotime('2020-12-31');

if ($timestamp1 < $timestamp2) {
    echo "Datum1 är tidigare än Datum2.";
}
```

Under huven, när två DateTime objekt jämförs, omvandlas båda objekten till ett format som kan jämföras; PHP tolkar automatiskt detta bakom kulisserna.

För att kunna jämföra två datum korrekt, bör samma tidszon användas för båda datumen, annars kan oavsiktliga fel uppstå.

Råd: Välj en metod som passar bäst för ditt projekt och dess krav.

## Se Även:

PHP ManualDateTime: https://www.php.net/manual/en/datetime.compare.php

PHP Manualstrtotime: https://www.php.net/manual/en/function.strtotime.php

PHP ManualDateTimeZone: https://www.php.net/manual/en/class.datetimezone.php

StackOverflow, "Compare two dates with JavaScript": https://stackoverflow.com/questions/492994/compare-two-dates-with-javascript