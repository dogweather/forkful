---
title:                "Analysering av en dato fra en streng"
aliases:
- /no/php/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:58.849429-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av en dato fra en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å analysere en dato fra en tekststreng i PHP innebærer å konvertere tekst som representerer en dato og/eller tid til et PHP `DateTime`-objekt eller andre dato-/tidsformater. Dette er avgjørende for datavalidering, manipulering, lagring og presentasjon, spesielt når man jobber med brukerinndata eller data fra eksterne kilder.

## Hvordan:

PHPs innebygde `DateTime`-klasse tilbyr et kraftfullt sett med funksjoner for å analysere og jobbe med datoer. Du kan opprette en `DateTime`-instans fra en dato-streng ved hjelp av konstruktøren, og deretter formatere den som nødvendig. Her er hvordan:

```php
$dateString = "2023-04-25 15:30:00";
$dateObject = new DateTime($dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Utdata: 2023-04-25 15:30:00
```

For å håndtere strenger som følger ikke-standard formater, kan du bruke `createFromFormat`-metoden, som lar deg spesifisere det eksakte formatet på inndatadatoen:

```php
$dateString = "25-04-2023 3:30 PM";
$dateObject = DateTime::createFromFormat('d-m-Y g:i A', $dateString);

echo $dateObject->format('Y-m-d H:i:s');
// Utdata: 2023-04-25 15:30:00
```

For mer kompleks parsing som kanskje ikke støttes direkte av `DateTime`, tilbyr PHP `strtotime`-funksjonen, som forsøker å analysere enhver engelsk tekstlig datotidsbeskrivelse til et Unix-tidsstempel:

```php
$timestamp = strtotime("next Thursday");
echo date('Y-m-d', $timestamp);
// Utdata vil variere avhengig av gjeldende dato, f.eks., "2023-05-04"
```

**Bruk av tredjepartsbiblioteker:**

Selv om PHPs innebygde funksjoner dekker et bredt spekter av bruksområder, kan du noen ganger trenge mer sofistikert parsingmuligheter. Carbon-biblioteket, en utvidelse av PHPs DateTime-klasse, gir et rikt sett med funksjoner for dato-/tidsmanipulering:

```php
require 'vendor/autoload.php';

use Carbon\Carbon;

$dateString = "Tomorrow";
$date = Carbon::parse($dateString);

echo $date->toDateTimeString();
// Utdata vil variere, f.eks., "2023-04-26 00:00:00"
```

Carbons `parse`-metode kan smart håndtere et mangfold av dato- og tidsformater, noe som gjør det til et uvurderlig verktøy for applikasjoner som krever fleksibel datoparsingfunksjonalitet.
