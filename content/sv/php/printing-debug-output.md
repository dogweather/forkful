---
title:                "Utskrift av felsökningsresultat"
html_title:           "PHP: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?:
Att skriva ut debug output är en teknik som används av programmerare för att söka och identifiera fel eller problem i sin kod. Det innebär att man skriver ut variabler eller meddelanden i koden för att se hur den utvecklas under körning och på så sätt kunna förstå vad som eventuellt går fel.

## Hur man gör:
Det är enkelt att skriva ut debug output i PHP. Använd bara funktionen `echo()` eller `print_r()`för att skriva ut en variabel eller ett meddelande. Till exempel:
```PHP
$namn = "Nisse";
echo "Hej " . $namn; //Output: Hej Nisse

$array = [1,2,3];
print_r($array); //Output: Array ([0] => 1, [1] => 2, [2] => 3)
```

## Djupdykning:
Metoden att skriva ut debug output har funnits i flera år och är fortfarande en populär teknik bland programmerare. En alternativ metod är att använda ett debuggingsverktyg som xdebug, som ger mer detaljerad information och möjlighet att stega igenom koden. Implementeringen av debug output är också beroende av programmiringsspråket och plattformen som används.

## Se också:
- [Mer om `echo()` och `print_r()`](https://www.php.net/manual/en/function.echo.php)
- [Guide till debugging i PHP](https://www.codecademy.com/articles/how-to-debug-php)
- [xdebug](https://xdebug.org/)