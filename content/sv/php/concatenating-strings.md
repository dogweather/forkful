---
title:                "Sammanslåning av strängar"
html_title:           "PHP: Sammanslåning av strängar"
simple_title:         "Sammanslåning av strängar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanslå strängar är en vanlig uppgift inom PHP-programmering. Genom att kombinera flera strängar till en enda kan du skapa dynamiska och anpassade meddelanden eller skapa mer läsbara koder.

## Så här gör du

Att sammanslå strängar i PHP är enkelt. Du kan använda operatorn "." för att sammanfoga två eller flera strängar tillsammans.

```PHP
echo "Hej, " . "världen!";
```

Resultatet av koden ovan blir "Hej, världen!". Du kan också använda variabler i sammanslagningen.

```PHP
$name = "John";
echo "Välkommen, " . $name . "!";
```

Det resulterande meddelandet blir "Välkommen, John!".

Du kan även sammanslå strängar med hjälp av funktionen "sprintf ()". Detta gör att du kan formatera ditt meddelande på ett mer organiserat sätt och lägga till variabler på specifika platser i strängen.

```PHP
$name = "Lisa";
$age = 25;
$message = sprintf("Hej, mitt namn är %s och jag är %d år gammal.", $name, $age);
echo $message;
```

Resultatet blir "Hej, mitt namn är Lisa och jag är 25 år gammal.".

## Deep Dive

När du sammanslår strängar i PHP måste du vara medveten om skillnaden mellan enkel- och dubbelcitationstecken. Enkla citationstecken tolkas bokstavligt, medan variabler eller specialtecken inuti dubbelcitationstecken tolkas som kod.

Tänk på följande exempel:

```PHP
$name = "Johanna";
echo 'Hej, $name!';
```

I det här fallet kommer variabeln $name inte att tolkas eftersom den finns inuti enkla citationstecken. Det resulterande meddelandet kommer att vara "Hej, $name!".

Men om vi använder dubbelcitationstecken istället:

```PHP
$name = "Johanna";
echo "Hej, $name!";
```

Resultatet blir "Hej, Johanna!". Variabeln $name tolkas som kod och dess värde ersätter variabeln i strängen.

## Se även

- [PHP Strängar](https://www.php.net/manual/en/language.types.string.php)
- [PHP sprintf () funktionen](https://www.php.net/manual/en/function.sprintf.php)
- [PHP Operators](https://www.php.net/manual/en/language.operators.php)