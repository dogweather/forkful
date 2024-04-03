---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:01.968609-07:00
description: "Hur man g\xF6r: PHP st\xF6der medf\xF6tt olika funktioner f\xF6r att\
  \ versalisera str\xE4ngar, var och en tj\xE4nar ett olika syfte. H\xE4r \xE4r hur\
  \ du kan anv\xE4nda dem: #."
lastmod: '2024-03-13T22:44:37.979971-06:00'
model: gpt-4-0125-preview
summary: "PHP st\xF6der medf\xF6tt olika funktioner f\xF6r att versalisera str\xE4\
  ngar, var och en tj\xE4nar ett olika syfte."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

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
