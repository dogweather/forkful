---
title:                "Jämförelse av två datum"
html_title:           "PHP: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum är en vanlig uppgift för programmerare, speciellt när man jobbar med webbutveckling. Det kan användas för att kolla vilket datum som är senast eller för att kontrollera om två händelser inträffat samtidigt.

## Hur man gör:

Här är två exempel på hur man kan jämföra två datum i PHP:

```PHP
$datum1 = "18 augusti 2021";
$datum2 = "25 augusti 2021";

if ($datum1 > $datum2) {
    echo "$datum1 är senare än $datum2.";
} elseif ($datum1 < $datum2) {
    echo "$datum1 är tidigare än $datum2.";
} else {
    echo "Båda datum är samma.";
}
```

Output: 18 augusti 2021 är tidigare än 25 augusti 2021.

```PHP
$nuvarandeDatum = date('Y-m-d');
$framtidDatum = date('Y-m-d', strtotime("+1 week"));

if ($nuvarandeDatum > $framtidDatum) {
    echo "Framtidsdatumet har redan passerat.";
} elseif ($nuvarandeDatum < $framtidDatum) {
    echo "Framtidsdatumet är fortfarande i framtiden.";
} else {
    echo "Framtidsdatumet är samma som dagens datum.";
}
```

Output: Framtidsdatumet är fortfarande i framtiden.

## Djupdykning:

Det finns flera sätt att jämföra datum i PHP, men det enklaste är att konvertera datumen till Unix-timestamps och sedan jämföra dem som heltal. Detta gör att man undviker problem med format och tidszoner.

Det finns också funktioner i PHP som kan göra mer avancerade jämförelser, som till exempel identifiera om ett datum är en skottår eller att jämföra endast månader eller år.

## Se även:

- [PHP Datetime funktioner](https://www.php.net/manual/en/book.datetime.php)
- [Stack Overflow diskussion om att jämföra datum i PHP](https://stackoverflow.com/questions/5992492/how-to-compare-two-dates-in-php)
- [PHP Datetime-tips](https://www.phpro.org/examples/Calculate-Days-Between-Two-Dates.html)