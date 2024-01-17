---
title:                "Att Göra en Sträng Med Stora Bokstäver"
html_title:           "PHP: Att Göra en Sträng Med Stora Bokstäver"
simple_title:         "Att Göra en Sträng Med Stora Bokstäver"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att "capitalizera" en sträng i programmering innebär att konvertera alla bokstäver i en sträng till stora bokstäver. Detta görs oftast för att standardisera text och öka läsbarheten. Det är också vanligt att göra detta för att matcha sökningar då de allra flesta programmeringspråk är "case-sensitive", vilket innebär att stora och små bokstäver skiljer sig åt.

## Hur man gör:
För att capitalizera en sträng i PHP kan du använda funktionen `strtoupper()` som står för "string to upper case". Detta fungerar på både enskilda tecken och hela strängar. Här är ett exempel:

```php
<?php
  $text = "denna text kommer capitalizeras";
  echo strtoupper($text);
?>

// Output:
DENNA TEXT KOMMER CAPITALIZERAS
```

Du kan också använda `ucwords()` funktionen för att capitalizera varje ord i en sträng. Detta är användbart om du vill ha första bokstaven i varje ord stor. Här är ett exempel:

```php
<?php
  $text = "detta är en mening som kommer capitalizeras";
  echo ucwords($text);
?>

// Output:
Detta Är En Mening Som Kommer Capitalizeras
```

## Mer djupgående:
Capitalizering av strängar har funnits med sedan de tidiga dagarna av programmering då text redan var case-sensitive. Olika programmeringspråk har olika metoder för att capitalizera strängar, såsom `capitalize()` i Ruby och `String.ToUpper()` i C#. Det är viktigt att använda rätt metod för det språk du arbetar i.

Det finns också alternativ till `strtoupper()` och `ucwords()`, såsom att manuellt loopa igenom varje tecken i en sträng och konvertera det till stora bokstäver. Detta kan dock bli tidskrävande och är därför inte det mest effektiva sättet att capitalizera en sträng.

I PHP kan `strtoupper()` och `ucwords()` även användas för att capitalizera UTF-8-tecken, vilket är användbart för flerspråkiga applikationer.

## Se även:
- PHP:s inbyggda funktioner för strängmanipulering: https://www.php.net/manual/en/ref.strings.php
- En grundläggande introduktion till PHP: https://www.w3schools.com/php/
- Inlägg om capitalizering av strängar på Stack Overflow: https://stackoverflow.com/questions/1784012/how-to-capitalize-the-first-letter-of-each-word-in-a-string