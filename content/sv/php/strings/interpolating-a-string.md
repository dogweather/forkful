---
title:                "Interpolera en sträng"
aliases: - /sv/php/interpolating-a-string.md
date:                  2024-01-20T17:51:26.930779-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/interpolating-a-string.md"
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
