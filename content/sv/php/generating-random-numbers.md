---
title:                "PHP: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

Varför: Att generera slumpmässiga nummer kan vara användbart för många olika ändamål inom PHP-programmering, såsom att skapa unika användar-ID:n eller att simulera slumpmässiga händelser.

Hur man gör det: Det finns flera olika metoder för att generera slumpmässiga nummer i PHP. En enkel metod är att använda funktionen "mt_rand()", vilket står för "Mersenne Twister Random". Den här funktionen tar två argument, det lägsta och högsta värdet som du vill ha för ditt slumpmässiga nummer. Låt oss titta på ett exempel:

```PHP
//generera ett slumpmässigt nummer mellan 1 och 10
$random_number = mt_rand(1, 10);
echo $random_number; // kommer att visa ett slumpmässigt nummer mellan 1 och 10 varje gång koden körs
```

En annan metod är att använda funktionen "rand()", som tar ett lägsta och högsta värde som argument och genererar ett slumpmässigt nummer mellan dessa värden. Här är ett exempel:

```PHP
//generera ett slumpmässigt nummer mellan 5 och 15
$random_number = rand(5, 15);
echo $random_number; // kommer att visa ett slumpmässigt nummer mellan 5 och 15 varje gång koden körs
```

Du kan också använda funktionen "array_rand()" för att slumpmässigt välja ett element från en array. Till exempel:

```PHP
//definiera en array med olika namn
$names = array("Lisa", "Olle", "Anna", "Johan", "Eva");
//generera ett slumpmässigt nummer för att välja ett namn från arrayen
$random_number = array_rand($names);
echo $names[$random_number]; // kommer att visa ett av namnen i $names-arrayen varje gång koden körs
```

Djupdykning: Bakom kulisserna använder PHP "srand()" -funktionen som sållar och startar Mersenne Twister-algoritmen för att generera slumpmässiga nummer. Det finns också möjlighet att ge ett "frö" som argument till "srand()" -funktionen för att få samma slumpmässiga nummer varje gång koden körs. Om du till exempel använder funktionen "mt_rand()" flera gånger i ditt program och vill ha samma nummer varje gång, kan du sätta en "srand()" -funktion vid programmet början och ge den samma "frö" som argument varje gång. Detta kan vara användbart för tester eller debugging.

Se också: För mer information om att generera slumpmässiga nummer i PHP, kolla in följande resurser:

- PHP-dokumentationen för "mt_rand()" - https://www.php.net/manual/en/function.mt-rand.php
- En tutorial om att skapa slumpmässiga nummer i PHP - https://www.w3schools.com/php/func_math_mt_rand.asp
- Forumdiskussion om att använda "srand()" och "mt_rand()" tillsammans - https://stackoverflow.com/questions/33844912/php-mt-rand-generates-the-same-sequence-of-random-numbers
- En förklaring av Mersenne Twister-algoritmen - https://en.wikipedia.org/wiki/Mersenne_Twister

Se även: För andra blogginlägg om PHP-programmering, ta en titt på våra andra artiklar på vår hemsida.