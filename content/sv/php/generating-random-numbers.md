---
title:    "PHP: Skapa slumpmässiga nummer"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

Varför generera slumpmässiga nummer med PHP?

Att kunna generera slumpmässiga nummer är en mycket användbar funktion inom programmering, speciellt inom webbutveckling. Det kan användas för allt från att skapa slumpmässiga lösenord till att simulera slumpmässiga händelser i spel. Med hjälp av PHP, kan du enkelt skapa dina egna slumpmässiga nummer för att använda i dina projekt.

Hur man genererar slumpmässiga nummer med PHP

För att kunna generera slumpmässiga nummer med PHP, behöver du använda funktionen "rand()" som är inbyggd i språket. Denna funktion tar emot två parametrar, det lägsta och högsta värdet för intervallet av de slumpmässiga numren du vill generera. Här är ett exempel på hur du kan använda "rand()" för att skapa fem slumpmässiga nummer mellan 1 och 10:

```PHP
<?php
for($i = 0; $i < 5; $i++){
    $random = rand(1, 10);
    echo $random . "\n";
}
?>
```

Detta kodblock kommer att ge dig en utskrift som ser ut så här:

```
5
2
9
8
3
```

Djupdykning i generering av slumpmässiga nummer

När du använder funktionen "rand()", är det viktigt att förstå att de genererade numren är pseudoslumpmässiga. Det betyder att de inte är helt slumpmässiga utan genereras utifrån en matematisk algoritm. Denna algoritm baseras på en "seed", som vanligtvis är tiden när funktionen kallas. Detta resulterar i att om du genererar slumpmässiga nummer i en snabb följd, kan de vara mindre slumpmässiga eftersom de baseras på samma "seed". En lösning på detta problem är att använda funktionen "mt_rand()" istället, som använder en annan algoritm som resulterar i mer slumpmässiga nummer.

En annan viktig punkt att komma ihåg är att funktionen "rand()" är beroende av konfigurationen av din server. Om du inte har tillgång till konfigurationen, kan du istället använda funktionen "getrandmax()", vilket ger dig det högsta talet som kan returneras av "rand()".

Se även

- PHPs officiella dokumentation för "rand()": https://www.php.net/manual/en/function.rand.php
- En artikel om pseudoslumpmässiga nummer: https://hackernoon.com/how-does-a-computer-generate-pseudo-random-numbers-9329bbfe0908
- Användning av slumpmässiga nummer i spel med PHP: https://www.simplifiedcoding.net/random-number-picker-php/