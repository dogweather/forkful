---
title:                "PHP: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför använda vanliga uttryck (regular expressions)?

Vanliga uttryck, även kända som regex, är ett mycket användbart verktyg för programmerare. Genom att använda regex kan man söka igenom text och matcha specifika mönster. Detta kan vara särskilt användbart när man behöver filtrera eller manipulera data från stora textmängder. Det är också ett bekvämt sätt att validera inmatade formulärdata eller uthämta information från webbsidor. 

## Så här använder du vanliga uttryck i PHP

För att använda regex i PHP, behöver man använda inbyggda funktioner som `preg_match()` eller `preg_replace()`. Dessa funktioner tar emot två parametrar: uttrycket man vill matcha och den sträng som man vill söka igenom. Här är ett exempel:

```PHP
$str = "Hej Swepish readers!";
if (preg_match("/Swedish/", $str)) {
  echo "Härligt att ni läser på svenska!";
} else {
  echo "Det är aldrig för sent att lära sig!";
}
```

Om vi kör detta kodblock kommer vi att få utskriften "Härligt att ni läser på svenska!". I detta exempel så letar vi efter ordet "Swedish" och om det finns i strängen, kommer `preg_match()` att returnera sant och skriva ut det positiva meddelandet.

Om man vill ändra eller ersätta delar av en sträng, kan man använda `preg_replace()` istället:

```PHP
$name = "Sara";
echo preg_replace("/Sara/", "Jenny", $name);
```

Detta kommer att skriva ut "Jenny" istället för "Sara". Men vad händer om man vill matcha flera mönster? Då kan man använda flera olika modifikatorer i slutet av regex-uttrycket. Till exempel kan man använda `i` för att ignorera skillnader mellan versaler och gemener, `g` för att matcha flera förekomster och `m` för att behandla en flerradig sträng som flera rader.

## Djupdykning i vanliga uttryck

Regex kan vara en djungel av specialtecken och modifikatorer, men det finns många resurser tillgängliga för att hjälpa dig lära dig mer om det. Här är några webbplatser som jag rekommenderar:

- [PHP:s officiella dokumentation om regex](https://www.php.net/manual/en/regexp.reference.php)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Regex101](https://regex101.com/)

Det är också viktigt att komma ihåg att regex har sina begränsningar och det kan ibland vara bättre att använda andra metoder för att hantera text. Men med rätt kunskap och övning kan regex vara en ovärderlig verktyg i din programmeringsrepertoar.

## Se även

Här är några andra artiklar som kan vara intressanta för dig:

- [Så här skapar du ett enkelt formulär i PHP](https://www.example.com/formulär-i-php)
- [Användbara PHP-funktioner för strängmanipulation](https://www.example.com/funktioner-for-strangmanipulation-i-php)
- [Fem vanliga misstag i PHP och hur man undviker dem](https://www.example.com/vanliga-misstag-i-php)