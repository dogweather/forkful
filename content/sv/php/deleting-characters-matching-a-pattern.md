---
title:    "PHP: Radera tecken som matchar ett mönster"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett visst mönster är en användbar funktion inom programmering när man behöver rengöra data eller filtrera ut vissa tecken från en textsträng.

## Hur man gör det

För att ta bort tecken som matchar ett visst mönster kan vi använda oss av PHP-funktionen `preg_replace()`. Den tar emot tre argument - mönstret att matcha, det sträng som vi vill söka igenom och det vi vill ersätta det matchade mönstret med. Här är ett exempel:

```PHP
$original_string = "Denna mening innehåller 4 siffror: 1234";
$pattern = '/\d/'; // här är mönstret för att matcha alla siffror
$replacement = ''; // här är vad vi vill ersätta siffrorna med, i detta fall ingenting
$new_string = preg_replace($pattern, $replacement, $original_string);

echo $new_string; // Output: Denna mening innehåller  siffror: 
```

Notera att vi använder sig av en så kallad "regex" (regular expression) för att definiera mönstret. Det finns många olika regex-mönster som kan matcha olika typer av tecken i en sträng. En bra resurs för att lära sig mer om dessa mönster är [RegExr](https://regexr.com/).

## Djupdykning

När vi använder `preg_replace()` så blir det nya strängen den ursprungliga strängen minus alla tecken som matchar det definierade mönstret. Om vi vill ta bort alla tecken utom siffror kan vi använda oss av regex-mönstret `/\D/`, där `\D` representerar alla icke-numeriska tecken.

Vi kan också använda oss av andra strängfunktioner, som `str_replace()`, för att ta bort specifika tecken eller teckenkombinationer. Det är viktigt att ha koll på vilka tecken som finns i den ursprungliga strängen och vilka som ska tas bort för att undvika oönskade resultat.

## Se även

Här är några användbara länkar för att lära dig mer om att ta bort tecken som matchar ett visst mönster i PHP:

- [PHP - Manipulating Strings](https://www.w3schools.com/php/php_ref_string.asp)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [PHP.net - preg_replace Documentation](https://www.php.net/manual/en/function.preg-replace.php)