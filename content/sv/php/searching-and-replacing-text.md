---
title:                "PHP: Söka och ersätta text"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en grundläggande uppgift inom programmering, oavsett vilket språk du arbetar med. Genom att använda en kombination av sök- och ersättningsfunktioner kan du effektivt bearbeta stora mängder text för att uppnå önskat resultat.

## Hur man gör det
Att söka och ersätta text i PHP är enkelt och det finns flera olika sätt att göra det på. Det vanligaste sättet är genom att använda str_replace() funktionen. Här är ett exempel:

```PHP
$str = "Hej, jag heter Johan.";
echo str_replace("Johan", "Lisa", $str);
```

Output:

Hej, jag heter Lisa.

Förutom str_replace() finns det också andra funktioner som kan användas för att söka och ersätta text, till exempel preg_replace() för att använda reguljära uttryck.

## Djupdykning
Om du vill ha mer kontroll över sök- och ersättningsprocessen kan du använda en mer komplex funktion som substr_replace(). Denna funktion låter dig ange en specifik del av texten som ska ersättas, istället för hela strängen. Här är ett exempel som använder substr_replace() för att ersätta det andra ordet i en sträng:

```PHP
$str = "Jag älskar att programera.";
echo substr_replace($str, "andas", 9, 12);
```

Output:

Jag älskar att andas.

Det är också viktigt att notera att sök- och ersättningsfunktionerna är "case sensitive" vilket betyder att de skiljer mellan stora och små bokstäver. Om du vill att din sökning och ersättning ska vara oberoende av detta kan du använda funktionen str_ireplace() istället.

## Se även
Här är några användbara resurser för dig som vill lära dig mer om hur man söker och ersätter text i PHP:

- [PHP manual](https://www.php.net/manual/en/function.str-replace.php)
- [W3Schools tutorial](https://www.w3schools.com/php/func_string_str_replace.asp)
- [Tutorialspoint guide](https://www.tutorialspoint.com/php/php_regular_expression.htm)