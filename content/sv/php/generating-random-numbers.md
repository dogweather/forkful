---
title:                "PHP: Generera slumpmässiga nummer"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför
Att generera slumpmässiga nummer är en viktig del av många PHP-program. Om du behöver skapa en unik identifierare, en slumpmässig sekvens i ett spel eller helt enkelt behöver slumpmässiga värden för testning av ditt program, kan du använda PHP:s inbyggda funktioner för att generera dessa nummer.

## Så här gör du
För att generera slumpmässiga nummer i PHP använder du funktionen `rand (int $min, int $max)`. Detta genererar ett nummer mellan `$min` och `$max`. Låt oss se några exempel:

```PHP
echo rand(10, 20);
// Output: Slumpmässigt nummer mellan 10 och 20

echo rand(0, 100);
// Output: Slumpmässigt nummer mellan 0 och 100

echo rand(5, 5);
// Output: 5 - eftersom det är det minsta och största värdet som anges
```

Du kan också använda `mt_rand ()` -funktionen, vilket genererar pseudoslumpmässiga nummer och anses vara snabbare än `rand ()`. Syntaxen är densamma som för `rand ()`:

```PHP
echo mt_rand(100, 1000);
// Output: Slumpmässigt nummer mellan 100 och 1000
```

För att generera slumpmässiga decimaltal kan du använda `mt_rand ()` tillsammans med `round ()`:

```PHP
echo round(mt_rand(10, 20) / 10, 1);
// Output: Slumpmässigt decimaltal mellan 1.0 och 2.0 med en decimal

echo round(mt_rand(-50, 50) / 10, 1);
// Output: Slumpmässigt decimaltal mellan -5.0 och 5.0 med en decimal
```

## Djupdykning
Bakom kulisserna använder PHP en algoritm för att generera slumpmässiga nummer baserat på en så kallad "seed", som är en startpunkt för algoritmen. Om du inte anger en seed för `mt_rand ()` kommer den att generera en slumpmässig seed varje gång den körs, vilket resulterar i olika nummer varje gång. Om du vill få samma slumpmässiga nummer varje gång kan du använda `mt_srand ()` för att ställa in en seed.

```PHP
mt_srand(42); // Ställ in seeden på 42
echo mt_rand();
// Output: 1592183196 - samma nummer genereras varje gång

mt_srand(42); // Ställ in seeden till samma värde igen
echo mt_rand();
// Output: 235713604 - genererar fortfarande det samma numret
```

Du kan också använda `mt_getrandmax ()` för att få det maximala värdet som kan genereras med `mt_rand ()` på din server.

## Se även
- [Generera slumpmässiga strängar i PHP](link1)
- [Användbara inbyggda funktioner i PHP](link2)
- [GitHub: PHP mt_rand ()](link3)