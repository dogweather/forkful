---
title:                "PHP: Extrahera substrängar"
simple_title:         "Extrahera substrängar"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Strängutvinning är en vanlig programmeringsteknik som används för att extrahera en del av en sträng. Detta kan vara användbart i många olika situationer, till exempel när du vill söka igenom en lång sträng efter ett specifikt ord eller när du vill manipulera en sträng för att få den att passa in på ett visst format.

## Så här gör du
Det enklaste sättet att extrahera en del av en sträng är genom att använda funktionen `substr()`. Den tar in tre parametrar: strängen som ska bearbetas, startpositionen för den del av strängen du vill extrahera och det valfria antalet tecken som ska extraheras.

```PHP
<?php
$str = "Välkommen till min blogg!";
echo substr($str, 10); // Output: min blogg!
```

Om du istället bara vill extrahera de första 10 tecknen av strängen kan du använda negativa värden för de sista två parametrarna, vilket gör att den del av strängen som är utanför det specificerade området ignoreras.

```PHP
<?php
$str = "Välkommen till min blogg!";
echo substr($str, 0, -7); // Output: Välkommen till
```

Det är också möjligt att använda regelbundna uttryck för att extrahera en del av en sträng. Detta ger dig ännu mer kontroll över vilka delar av strängen du vill hämta.

```PHP
<?php
$str = "Hemsida: www.example.com";
preg_match('/w{3}.(.+?)\/$/i', $str, $matches); // Hitta delen av strängen efter "www." och fram till "/"-tecknet
echo $matches[1]; // Output: example
```

## Deep Dive
För mer komplexa extraheringsbehov kan det vara användbart att använda funktionerna `strpos()` och `strrpos()` för att hitta positionerna för den del av strängen du vill hämta. Dessa funktioner returnerar det första respektive sista förekomsten av ett visst tecken eller en delsträng.

Om du behöver hantera flera förekomster av en delsträng kan du använda `explode()` för att dela upp strängen i en array vid varje förekomst av den delsträng du letar efter.

## Se även
- [PHP Manual: substr](https://www.php.net/manual/en/function.substr.php)
- [PHP Manual: preg_match](https://www.php.net/manual/en/function.preg-match.php)
- [PHP Manual: strpos](https://www.php.net/manual/en/function.strpos.php)
- [PHP Manual: strrpos](https://www.php.net/manual/en/function.strrpos.php)
- [PHP Manual: explode](https://www.php.net/manual/en/function.explode.php)