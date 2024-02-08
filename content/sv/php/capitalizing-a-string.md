---
title:                "Gör om en sträng till versaler"
aliases:
- sv/php/capitalizing-a-string.md
date:                  2024-02-03T19:06:01.968609-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att versalisera en sträng innebär att ändra det första tecknet i given text till versal, och säkerställer att meningar, titlar eller egennamn börjar korrekt i en datamängd. Programmerare utför ofta versalisering av strängar för datanormalisering, för att förbättra läsbarheten eller för att säkerställa konsekvens i användarinput eller textuell databehandling.

## Hur man gör:
PHP stöder medfött olika funktioner för att versalisera strängar, var och en tjänar ett olika syfte. Här är hur du kan använda dem:

### Versalisera första bokstaven i en sträng:

```php
$string = "hello, world!";
$capitalizedString = ucfirst($string);
echo $capitalizedString; // Skriver ut: Hello, world!
```

### Versalisera första bokstaven i varje ord:

```php
$string = "hello, world!";
$capitalizedWords = ucwords($string);
echo $capitalizedWords; // Skriver ut: Hello, World!
```

### Konvertera hela strängen till versaler:

```php
$string = "hello, world!";
$upperCaseString = strtoupper($string);
echo $upperCaseString; // Skriver ut: HELLO, WORLD!
```

För scenarion som kräver mer anpassning eller lösningar från tredje part, kan bibliotek som `mbstring` (för flerbyte strängar) användas, speciellt vid hantering av internationalisering där tecken kan sträcka sig utöver det grundläggande ASCII-setet.

### Använda mbstring för att versalisera UTF-8 strängar:

Se till att du har `mbstring`-tillägget aktiverat i din PHP-konfiguration, sedan:

```php
$string = "élégant";
$capitalizedString = mb_convert_case($string, MB_CASE_TITLE, "UTF-8");
echo $capitalizedString; // Skriver ut: Élégant
```

Detta tillvägagångssätt hjälper till att korrekt versalisera strängar som inkluderar icke-ASCII tecken, i enlighet med nyanserna i olika språk.
