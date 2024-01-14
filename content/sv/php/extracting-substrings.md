---
title:    "PHP: Extrahering av delsträngar"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/extracting-substrings.md"
---

{{< edit_this_page >}}

# Varför extrahera substrings?

Att extrahera substrings är en viktig färdighet i PHP-programmering eftersom det låter dig fånga delar av en textsträng utan att behöva använda hela strängen. Detta gör det möjligt att manipulera, jämföra och analysera texter på ett mer effektivt sätt.

## Så här extraherar du substrings i PHP

För att extrahera en substring i PHP behöver du använda en inbyggd funktion, `substr()`. Denna funktion tar in en sträng, en startposition och en valfri längd som parametrar. Här är ett exempel på hur du skulle använda `substr()` för att extrahera en del av en textsträng:

```PHP
$text = "Välkommen till min blogg!";
$substring = substr($text, 10, 5);
echo $substring;
```

I detta exempel kommer substringsen som extraheras från `$text` vara "min blogg". Startpositionen 10 indikerar att vi vill börja extrahera efter det tionde tecknet (räknat från noll) och längden 5 berättar för funktionen att vi vill ha fem tecken i vår extraherade del.

Om du istället inte anger en längd, kommer `substr()` att extrahera hela resten av strängen från och med startpositionen. Här är ett exempel på detta:

```PHP
$text = "Ta en kopp kaffe och stanna en stund.";
$substring = substr($text, 11);
echo $substring;
```

Funktionen kommer nu att extrahera allt efter det elfte tecknet, vilket ger oss "kopp kaffe och stanna en stund.". Det är viktigt att komma ihåg att substrings alltid räknas från noll, så i detta fall är det tecknet "k" som är det elfte tecknet.

## Djupdykning i substrings

Det finns ytterligare några koncept som är viktiga att notera när det gäller att extrahera substrings i PHP. En av dessa är negativa startpositioner, vilket låter dig extrahera en del av strängen från slutet istället för från början. Till exempel, om vi skulle använda `-7` som startposition i det första exemplet ovan så skulle vi fått samma resultat som att börja från position 10 då PHP kommer att räkna bakifrån om en negativ siffra används.

En annan viktig aspekt är användningen av enskilda tecken jämfört med byte-positioner när det gäller att extrahera substrings från icke-engelska språk. Detta beror på att tecknen i dessa språk kan ha mer än ett byte och därför måste man använda sig av specifika funktioner för att hantera sådana situationer.

## Se även

- [PHP's officiella dokumentation för substrings](https://www.php.net/manual/en/function.substr.php)
- [En bloggpost om substrings på svenska](https://medium.com/@johanessjostrand/php-substring-571d2f9936e0)