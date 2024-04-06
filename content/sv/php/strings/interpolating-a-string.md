---
date: 2024-01-20 17:51:26.930779-07:00
description: "S\xE5 h\xE4r g\xF6r du: Str\xE4nginterpolation har funnits i PHP sedan\
  \ tidiga versioner och \xE4r en favorit p\xE5 grund av dess enkelhet. Alternativ\
  \ till interpolation\u2026"
lastmod: '2024-04-05T21:53:39.323257-06:00'
model: gpt-4-1106-preview
summary: "Str\xE4nginterpolation har funnits i PHP sedan tidiga versioner och \xE4\
  r en favorit p\xE5 grund av dess enkelhet."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Så här gör du:
```PHP
// Grundläggande interpolation
$namn = "Erik";
$hälsning = "Hej, $namn!";
echo $hälsning; // Output: Hej, Erik!

// Komplex interpolation med klamrar
$artikel = "böcker";
$meddelande = "Jag har {$artikel}na du frågade efter.";
echo $meddelande; // Output: Jag har böckerna du frågade efter.
```

## Fördjupning
Stränginterpolation har funnits i PHP sedan tidiga versioner och är en favorit på grund av dess enkelhet. Alternativ till interpolation inkluderar konkatenering med `.` operatören eller funktionen `sprintf()`. Konkatenering kan bli rörigt när många variabler är involverade, medan `sprintf()` erbjuder formateringskontroll. Interpolation utförs under exekveringstid och innebär att PHP parsear strängen och ersätter variabler med deras värden.

## Se även:
- PHPs officiella dokumentation om strängar: [php.net/manual/en/language.types.string.php](https://www.php.net/manual/en/language.types.string.php)
- En artikel om sprintf() och dess användningsområden: [php.net/manual/en/function.sprintf.php](https://www.php.net/manual/en/function.sprintf.php)
