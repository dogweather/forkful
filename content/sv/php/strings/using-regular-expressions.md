---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:45.583477-07:00
description: "Regulj\xE4ra uttryck (regex) i PHP \xE4r m\xF6nster som anv\xE4nds f\xF6\
  r att matcha teckenkombinationer i str\xE4ngar, vilket m\xF6jligg\xF6r avancerade\
  \ s\xF6k-och-ers\xE4tt-\u2026"
lastmod: '2024-02-25T18:49:36.288725-07:00'
model: gpt-4-0125-preview
summary: "Regulj\xE4ra uttryck (regex) i PHP \xE4r m\xF6nster som anv\xE4nds f\xF6\
  r att matcha teckenkombinationer i str\xE4ngar, vilket m\xF6jligg\xF6r avancerade\
  \ s\xF6k-och-ers\xE4tt-\u2026"
title: "Att anv\xE4nda regulj\xE4ra uttryck"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck (regex) i PHP är mönster som används för att matcha teckenkombinationer i strängar, vilket möjliggör avancerade sök-och-ersätt-operationer och datavalidering. Programmerare utnyttjar regex för dess kraft och flexibilitet vid tolkning av text, validering av formulär eller skrapning av webbdata, vilket gör det till ett ovärderligt verktyg i en utvecklares arsenal.

## Hur man gör:

PHP stöder reguljära uttryck genom PCRE-biblioteket (Perl Compatible Regular Expressions), som erbjuder en rik uppsättning funktioner. Så här använder du dem:

### Matcha ett mönster:

För att kontrollera om ett mönster finns inom en sträng, använd `preg_match()`. Denna funktion returnerar 1 om mönstret hittades i strängen och 0 om inte.

```php
if (preg_match("/\bweb\b/i", "PHP är ett skriptspråk för webben")) {
    echo "En matchning hittades.";
} else {
    echo "Ingen matchning hittades.";
}
// Utdata: En matchning hittades.
```

### Hitta alla matchningar:

`preg_match_all()` används när du behöver hitta alla förekomster av ett mönster inom en sträng.

```php
$text = "katter och hundar";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// Utdata: Array ( [0] => katter [1] => och [2] => hundar )
```

### Ersätta text:

För att ersätta text som matchar ett reguljärt uttryck används `preg_replace()`. Det är otroligt kraftfullt för formatering och rensning av data.

```php
$originalText = "15 april, 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// Utdata: 151,2003
```

### Dela upp strängar:

Du kan dela en sträng i en array med `preg_split()`, genom att specificera ett mönster för avgränsaren.

```php
$text = "PHP är, ett extremt populärt, skriptspråk";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// Utdata: Array ( [0] => PHP är [1] => ett extremt populärt [2] => skriptspråk )
```

Förutom detta kan ramverk och bibliotek som Symfony’s `Finder`-komponent eller Laravels samling av hjälpfunktioner erbjuda ett mer bekvämt abstraktionslager för komplexa regex-mönster och uppgifter. Dock är det avgörande att förstå och använda PHP:s inbyggda PCRE-funktioner för effektiv textbehandling och validering direkt inom PHP-skript.
