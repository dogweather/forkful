---
title:    "PHP: Söka och ersätta text"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Så kanske du undrar varför det är så viktigt att kunna söka och ersätta text i PHP? Svaret är enkelt - det sparar tid och gör kodningen mer effektiv! Istället för att manuellt gå igenom en hel fil och ändra all förekommande text, kan du enkelt använda en sök- och ersättningsfunktion för att få jobbet gjort på bara några sekunder.

## Hur man gör
För att kunna söka och ersätta text i PHP behöver du använda funktionen `str_replace()`. Denna funktion tar in tre parametrar - söksträngen, ersättningssträngen och den ursprungliga strängen som du vill göra ändringar i. Här är ett exempel på hur det kan se ut:

```PHP
$text = "Hej världen!";
echo str_replace("världen", "Sverige", $text);
```

Outputen av detta kodsnutt skulle vara "Hej Sverige!". Som ni ser har funktionen sökt efter ordet "världen" i vår sträng och ersatt det med "Sverige". Detta kan även göras på flera ställen i en sträng samtidigt, genom att sätta sök- och ersättningssträngen i en array istället.

## Djupdykning
Nu när vi vet hur vi enkelt kan söka och ersätta text i PHP, kan vi gå lite djupare in i ämnet. En annan användbar funktion för detta ändamål är `preg_replace()`, som använder sig av reguljära uttryck för att söka och ersätta text. Detta ger oss mer flexibilitet och möjlighet att göra mer avancerade sökningar. Exempelvis kan man söka efter ett visst mönster, istället för ett exakt ord.

En annan anledning till varför det är viktigt att kunna söka och ersätta text är att det gör det enklare att underhålla koden. Om du plötsligt behöver ändra namnet på en variabel eller funktion, kan du enkelt göra det på alla ställen i koden genom att använda sök- och ersättningsfunktionen.

## Se även
- [PHP Manual - str_replace()](https://www.php.net/manual/en/function.str-replace.php)
- [PHP Manual - preg_replace()](https://www.php.net/manual/en/function.preg-replace.php)
- [Fler tips för effektiv programmering i PHP](https://www.mindk.com/blog/15-tips-to-make-your-php-code-more-efficient/)