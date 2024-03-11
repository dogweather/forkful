---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:01.968609-07:00
description: "Att versalisera en str\xE4ng inneb\xE4r att \xE4ndra det f\xF6rsta tecknet\
  \ i given text till versal, och s\xE4kerst\xE4ller att meningar, titlar eller egennamn\
  \ b\xF6rjar\u2026"
lastmod: '2024-03-11T00:14:11.350097-06:00'
model: gpt-4-0125-preview
summary: "Att versalisera en str\xE4ng inneb\xE4r att \xE4ndra det f\xF6rsta tecknet\
  \ i given text till versal, och s\xE4kerst\xE4ller att meningar, titlar eller egennamn\
  \ b\xF6rjar\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
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
