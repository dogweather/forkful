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

## Vad & Varför?
"Att extrahera substrängar" är när du plockar ut en del av en sträng, istället för hela strängen. Det är en vanlig uppgift inom programmering, eftersom det låter dig manipulera data på ett effektivt sätt.

## Så här gör du:
För att extrahera en substräng i PHP kan du använda funktionen `substr()`. Till exempel, om du har en sträng som heter `ord` och vill plocka ut de tre första tecknen, kan du använda följande kod:
```PHP
$ord = "hej";
echo substr($ord, 0, 3);
```
Detta skulle resultera i utmatningen "hej".

Om du vill plocka ut tecken från slutet av en sträng, kan du använda funktionen `substr()` tillsammans med `strlen()`, som ger längden på strängen. Till exempel, om du vill plocka ut de tre sista tecknen från strängen `hej-hopp`, kan du använda följande kod:
```PHP
$hejhop = "hej-hopp";
echo substr($hejhop, strlen($hejhop)-3, 3);
```
Detta skulle resultera i utmatningen "opp".

## Djupdykning:
Extrahering av substrängar är en grundläggande funktion inom programmering, och används ofta för att manipulera data eller filtrera ut relevant information från en större sträng. Innan funktionen `substr()` fanns tillgänglig i PHP, användes funktionen `eregi()` för att göra samma sak, men den är nu depricerad.

Det finns också andra sätt att extrahera substrängar i PHP, såsom att använda regler för reguljära uttryck med funktionen `preg_match()`. Detta är mer avancerat och används ofta när du behöver söka efter ett mönster i en sträng.

Att använda funktionen `substr()` är enkelt och effektivt, men det är alltid bra att ha kännedom om andra sätt att extrahera substrängar för att kunna använda den lämpligaste metoden för din specifika uppgift.

## Se även:
- PHP manual för [substr()](https://www.php.net/manual/en/function.substr.php)
- PHP manual för [preg_match()](https://www.php.net/manual/en/function.preg-match.php)