---
title:                "PHP: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering och kan vara till nytta för många olika ändamål. Det kan inkludera att byta ut felaktig eller föråldrad information, formatera text på ett enhetligt sätt eller göra omfattande ändringar i en stor textmängd.

## Hur man gör det

Det finns flera sätt att söka och ersätta text i PHP, men en av de enklaste metoderna är att använda en inbyggd funktion som heter `str_replace()`. Den tar tre parametrar: sökordet som ska ersättas, det nya ordet som ska sättas in samt texten som ska ändras.

```PHP
$text = "Hej världen!";
$ny_text = str_replace("världen", "världen av programmering", $text);
echo $ny_text;
```

Koden ovan kommer att ge följande utmatning: `Hej världen av programmering!` Ett annat sätt är att använda reguljära uttryck, vilket ger mer flexibilitet och möjlighet att söka efter mönster istället för en specifik textsträng.

```PHP
$text = "Lorem ipsum dolor sit amet, consectetur adipiscing elit.";
$ny_text = preg_replace("/[a-z]/", "X", $text);
echo $ny_text;
```

Koden ovan kommer att ersätta alla små bokstäver i texten med stora bokstäver, vilket ger följande utmatning: `XXXXXX XXXXX XXXXX XXX XXXXXXXXXXXXXX XXXXXXXXXXX XXXXX.` Det finns även andra funktioner som `str_ireplace()` som är antingen skiftlägeskänslig eller inte, beroende på behovet.

## Djupdykning

Att söka och ersätta text innebär inte bara byte av ord, utan kan också användas för att manipulera och hantera text på olika sätt. Det kan inkludera att ta bort eller lägga till tecken, använda reguljära uttryck för att söka efter mönster eller till och med utföra flera ersättningar i en text på samma gång.

En annan användbar funktion är `strtr()`, som erbjuder möjligheten att byta ut flera ord samtidigt enligt ett angivet mönster och ersättning. Till exempel:

```PHP
$text = "Jag tycker om äpplen och bananer.";
$ny_text = strtr($text, ["äpplen" => "päron", "bananer" => "apelsiner"]);
echo $ny_text;
```

Koden ovan kommer att ge följande utmatning: `Jag tycker om päron och apelsiner.` Det finns också många olika PHP-funktioner och bibliotek som kan hjälpa till med mer avancerad textmanipulering, som tex