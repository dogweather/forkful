---
title:                "PHP: Jämförande av två datum"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför jämföra två datum i PHP?

Att jämföra två datum är en vanlig uppgift inom programmering och kan vara användbart av olika anledningar. Det kan exempelvis hjälpa till att visa data i en kronologisk ordning eller filtera ut gamla poster. I denna bloggpost kommer vi att visa dig hur du kan jämföra två datum i PHP.

## Så här gör du

Det finns flera sätt att jämföra datum i PHP, men det enklaste sättet är att använda inbyggda funktioner som `strtotime()` eller `DateTime` klassen.

### `strtotime()`

Med `strtotime()`-funktionen kan du omvandla en sträng med datum till ett Unix-timestamp, vilket är ett antal sekunder som gått sedan 1 januari 1970. Detta låter dig sedan jämföra två datum genom att helt enkelt jämföra timestampen för varje datum.

Här är ett exempel på hur du kan använda `strtotime()` för att jämföra två datum:

```PHP
$datum1 = '2019-06-20';
$datum2 = '2019-06-25';

if (strtotime($datum1) < strtotime($datum2)) {
    echo 'Datum 1 är före Datum 2';
} elseif (strtotime($datum1) > strtotime($datum2)) {
    echo 'Datum 1 är efter Datum 2';
} else {
    echo 'Båda datumen är samma';
}
```

I detta exempel använder vi if-satser för att jämföra timestampen för varje datum. Resultatet kommer att vara antingen "Datum 1 är före Datum 2", "Datum 1 är efter Datum 2" eller "Båda datumen är samma", beroende på vilket datum som är före eller efter det andra.

### `DateTime` klassen

En annan vanlig metod för att jämföra datum i PHP är att använda `DateTime` klassen. Detta är en fördelaktig metod eftersom den låter dig jämföra datum med hjälp av flera olika metoder, som `diff()` för att räkna ut skillnaden mellan två datum och `format()` för att formatera datum på olika sätt.

Här är ett exempel på hur du kan använda `DateTime` klassen för att jämföra två datum:

```PHP
$datum1 = new DateTime('2019-06-20');
$datum2 = new DateTime('2019-06-25');

if ($datum1 < $datum2) {
    echo 'Datum 1 är före Datum 2';
} elseif ($datum1 > $datum2) {
    echo 'Datum 1 är efter Datum 2';
} else {
    echo 'Båda datumen är samma';
}
```

I detta exempel skapar vi två nya instanser av `DateTime` klassen och använder sedan if-satser för att jämföra dem. Resultatet kommer att vara detsamma som med `strtotime()`-metoden.

## Djupdykning

Nu när du vet hur du kan jämföra två datum i PHP är det även värt att nämna att det finns flera andra metoder för att jämföra datum, som `checkdate()` för att kontrollera om ett datum är giltigt och `time()` för att få den nuvarande tiden i form av en timestamp.

Det kan även vara värt att undersöka hur PHP hanterar tidzoner när man arbetar med datum och tider.

## Se även

Här är några relaterade länkar som kan vara till hjälp:

- [strtotime() dokumentation](https://www.php.net/manual/en/function.strtotime.php)
- [DateTime klassen dokumentation](https://www.php.net/manual/en/class.datetime.php)
- [PHP datumfunktioner](https://www.php.net/manual/en/ref.datetime.php)