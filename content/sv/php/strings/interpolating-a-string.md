---
date: 2024-01-20 17:51:26.930779-07:00
description: "Interpolera en str\xE4ng inneb\xE4r att infoga variabelv\xE4rden direkt\
  \ i str\xE4ngen. Programmerare g\xF6r detta f\xF6r att smidigt kunna bygga dynamiska\
  \ texter utan att\u2026"
lastmod: 2024-02-19 22:04:57.205526
model: gpt-4-1106-preview
summary: "Interpolera en str\xE4ng inneb\xE4r att infoga variabelv\xE4rden direkt\
  \ i str\xE4ngen. Programmerare g\xF6r detta f\xF6r att smidigt kunna bygga dynamiska\
  \ texter utan att\u2026"
title: "Interpolera en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Interpolera en sträng innebär att infoga variabelvärden direkt i strängen. Programmerare gör detta för att smidigt kunna bygga dynamiska texter utan att behöva bryta upp strängen.

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
