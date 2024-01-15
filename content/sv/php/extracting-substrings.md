---
title:                "Extrahering av substrängar"
html_title:           "PHP: Extrahering av substrängar"
simple_title:         "Extrahering av substrängar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Ibland behöver vi bara använda en del av en sträng istället för hela. Det kan vara för att hämta specifik information, utföra olika manipulationer eller helt enkelt för att göra koden mer läsbar. I PHP kallas denna process för "att extrahera substrängar" och det är en användbar teknik att lära sig.

## Så här gör du

För att extrahera substrängar i PHP använder vi inbyggda funktioner. Den vanligaste metoden är `substr()`, som tar emot tre parametrar: strängen som ska extraheras, startpositionen och optionellt en längd på den önskade substrängen. Låt oss se på ett exempel:

```PHP
$string = "Jag älskar att koda i PHP!";
$substring = substr($string, 11); // Startar på position 11
echo $substring; // Resultat: koda i PHP!
```

Om vi vill ha en specifik längd på vår substräng kan vi lägga till en tredje parameter, till exempel:

```PHP
$string = "Jag är en PHP ninja!";
$substring = substr($string, 9, 4); // Startar på position 9 och är 4 tecken lång
echo $substring; // Resultat: PHP
```

Vi kan också använda funktionen `mb_substr()` för att extrahera substrängar medan vi bevarar den teckenkodning vi använder. Till exempel:

```PHP
$string = "Jag talar flytande svenska!";
$substring = mb_substr($string, 5, 8); // Startar på position 5 och är 8 tecken lång
echo $substring; // Resultat: flytande
```

Det är viktigt att notera att startpositioner räknas från 0 istället för 1, och en negativ startposition betyder att räkna bakifrån. Så om vi vill extrahera den sista delen av en sträng, kan vi göra det genom att använda `-1` som startposition, till exempel:

```PHP
$string = "Jag är en kodningsrockstjärna!";
$substring = substr($string, -10); // Startar 10 tecken bakifrån
echo $substring; // Resultat: kodningsrockstjärna!
```

## Djupdykning

I bakgrunden använder PHP funktionen `mb_substr()` för att säkerställa att substrängen alltid har rätt teckenkodning, speciellt om vi arbetar med flerspråkiga strängar. Om du vill lära dig mer om hur den här funktionen fungerar kan du kolla på dokumentationen [här](https://www.php.net/manual/en/function.mb-substr.php).

Utöver `substr()` och `mb_substr()` finns det också andra inbyggda funktioner som kan vara användbara för att extrahera specifika delar av en sträng, som till exempel `strpos()`, `strrpos()` och `str_replace()`. Det är alltid en bra idé att läsa på dokumentationen för att utnyttja alla verktyg som PHP har att erbjuda när det gäller att hantera strängar.

## Se även

Här är några användbara länkar som kan hjälpa dig att lära dig mer om substrängar i PHP:

- [PHP Manual: substr()](https://www.php.net/manual/en/function.substr.php)
- [PHP Manual: mb_substr()](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP Manual: strpos()](https://www.php.net/manual/en/function.strpos.php)
- [PHP Manual: strrpos()](https://www.php.net/manual/en/function.strrpos.php)
- [PHP Manual: str_replace()](https://www.php.net/manual/en/function.str-replace.php)

Nu är det din tur att experimentera med substrängar och upptäcka alla möjligheter de kan ge! Lycka till!