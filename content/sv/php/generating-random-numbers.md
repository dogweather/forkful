---
title:                "Generera slumpmässiga tal"
html_title:           "PHP: Generera slumpmässiga tal"
simple_title:         "Generera slumpmässiga tal"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer är en vanlig praxis inom programmering. Det innebär att skapa en serie av nummer som inte följer någon specifik ordning eller mönster. Det kan användas för olika ändamål, såsom att skapa slumpvisa lösenord, generera unika IDs eller simulera slumpmässiga händelser i ett spel.

## Så här gör du:
Det finns flera sätt att generera slumpmässiga nummer i PHP, men den enklaste metoden är att använda funktionen `rand (min, max)` som tar emot två parametrar - ett minimumvärde och ett maximumvärde. Funktionen returnerar ett slumpmässigt heltal inom det angivna intervallet. Låt oss titta på ett exempel:

```
$random_number = rand(1, 10);
echo $random_number; // output: kan vara något av följande nummer: 1, 2, 3, 4, 5, 6, 7, 8, 9 eller 10.
```

Du kan även använda `mt_rand (min, max)` för att generera slumpmässiga nummer med ännu bättre slumpmässighet. Denna funktion använder en mer avancerad algoritm och är därför lite långsammare. Den används på samma sätt som `rand ()` funktionen.

## Djupdykning:
Randfunktionerna i PHP använder en algoritm som kallas Mersenne Twister för att skapa slumpmässiga nummer. Detta är den vanligaste algoritmen som används för slumpmässig nummergenerator och är känd för att producera högkvalitativa slumpmässiga nummer.

Alternativet till att använda PHP:s inbyggda funktioner är att använda en tredjepartsbibliotek som OpenSSL eller RandomLib för ännu mer säkra slumpmässiga nummer.

Det är också viktigt att komma ihåg att de genererade slumpmässiga numren inte är helt slumpmässiga, utan är baserade på en matematisk algoritm. Detta innebär att de kan förutsägas och bör inte användas för kryptografiska ändamål.

## Se även:
- [PHP:s officiella dokumentation för randfunktioner](https://www.php.net/manual/en/function.rand.php)
- [Random number generation in PHP by David Walsh](https://davidwalsh.name/php-random-number-generator)
- [Alternativa sätt att generera slumpmässiga nummer i PHP](https://davidwalsh.name/php-random-number-generator-alternatives)