---
title:    "PHP: Generera slumpmässiga nummer"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Varför

Många gånger i vår kod behöver vi slumpvisa nummer för att skapa unika ID:er, generera slumpmässiga färger eller bara som grund för en spellogik. I den här bloggposten kommer vi att utforska hur du kan skapa slumpmässiga nummer i PHP och ge dig lite djupare förståelse för processen.

## Så här

För att generera slumpmässiga nummer i PHP använder vi funktionen `rand()`. Denna funktion tar två parametrar, `min` och `max`, vilka definierar intervallet som det slumpmässiga numret ska genereras i.

```PHP
// Generera ett slumpmässigt heltal mellan 1 och 10
$randomNumber = rand(1, 10);

// Generera ett slumpmässigt decimaltal mellan 0 och 1
$randomDecimal = rand() / getrandmax();
```

Vi kan också använda `mt_rand()` för att generera mer säkra slumpmässiga nummer. Denna funktion tar också två parametrar, men har en bättre algoritm för att skapa slumpmässiga nummer.

```PHP
// Generera ett slumpmässigt heltal mellan 1 och 10
$randomNumber = mt_rand(1, 10);

// Generera ett slumpmässigt decimaltal mellan 0 och 1
$randomDecimal = mt_rand() / mt_getrandmax();
```

Nu när vi kan generera slumpmässiga nummer, låt oss använda det på ett konkret exempel. Vi ska skapa ett enkelt tärningsspel där datorn slår tärningen och vi gissar på om det blev udda eller jämnt.

```PHP
$computerRoll = rand(1, 6); // Datorn slår tärningen
$userGuess = "odd"; // Vår gissning

if ($computerRoll % 2 == 0) { // Om datorns tärning blev jämn
    $computerGuess = "even";
} else { // Om datorns tärning blev udda
    $computerGuess = "odd";
}

if ($userGuess == $computerGuess) { // Om vår gissning är samma som datorns
    echo "Grattis, du gissade rätt! Datorn slog en " . $computerRoll;
} else { // Om vår gissning är annorlunda från datorns
    echo "Tyvärr, du gissade fel. Datorn slog en " . $computerRoll;
}
```

## Djupdykning

Det finns flera olika algoritmer som används för att generera slumpmässiga nummer. Men den mest grundläggande metoden är genom att använda en matematisk formel som kallas "linear congruential generator". Det är en enkel formel som använder ett startvärde och en multiplikator för att generera ett nytt värde varje gång.

Det är viktigt att komma ihåg att även om dessa funktioner genererar nummer som är tillräckligt slumpmässiga för de flesta användningsområden, är de inte helt säkra för kryptografiska syften.

## Se även

- [Dokumentation för rand()](https://www.php.net/manual/en/function.rand.php)
- [Dokumentation för mt_rand()](https://www.php.net/manual/en/function.mt-rand.php)
- [Tutorial om generering av slumpmässiga nummer i PHP](https://code.tutsplus.com/tutorials/random-number-generation-in-php--cms-27020)