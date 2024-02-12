---
title:                "Analysera ett datum från en sträng"
aliases: - /sv/php/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:54.977561-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysera ett datum från en sträng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng i PHP innebär att konvertera text som representerar ett datum och/eller tid till ett PHP `DateTime`-objekt eller andra datum/tid-format. Detta är avgörande för datavalidering, manipulation, lagring och presentationssyften, särskilt när man arbetar med användarinmatning eller data från externa källor.

## Hur man gör:

PHP:s inbyggda `DateTime`-klass tillhandahåller en kraftfull uppsättning funktioner för att tolka och arbeta med datum. Du kan skapa en `DateTime`-instans från en datumsträng med konstruktören och sedan formatera den efter behov. Så här gör du:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Utdata: 2023-04-25 15:30:00
```

För att hantera strängar som följer icke-standardiserade format, kan du använda metoden `createFromFormat`, som låter dig specificera det exakta formatet på indatatdatumet:

```php
$dateString = "25-04-2023 3:30 EM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Utdata: 2023-04-25 15:30:00
```

För mer komplex tolkning som kanske inte direkt stöds av `DateTime`, erbjuder PHP funktionen `strtotime`, som försöker tolka en engelsk textuell datumtidbeskrivning till ett Unix-tidsstämpel:

```php
$timestamp = strtotime("nästa torsdag");
echo date('Y-m-d', $timestamp);
// Utdata kommer att variera beroende på aktuellt datum, t.ex. "2023-05-04"
```

**Användning av tredjepartsbibliotek:**

Medan PHP:s inbyggda funktioner täcker ett brett spektrum av användningsfall, kan du ibland behöva mer sofistikerade tolkningsfunktioner. Carbonbiblioteket, en utökning av PHP:s DateTime-klass, erbjuder en rik uppsättning funktioner för datum/tid-manipulering:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Imorgon";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// Utdata kommer att variera, t.ex., "2023-04-26 00:00:00"
```

Carbons `parse`-metod kan smart hantera en mängd datum- och tidsformat, vilket gör det till ett ovärderligt verktyg för applikationer som kräver flexibel datumtolkningsfunktion.
