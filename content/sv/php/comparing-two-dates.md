---
title:    "PHP: Jämförelse av två datum"
keywords: ["PHP"]
---

{{< edit_this_page >}}

# Varför

Att jämföra två datum är en viktig del av programmering eftersom det tillåter oss att hantera och manipulera datum på ett effektivt sätt. Det är också en viktig färdighet när man arbetar med databaser och behöver filtrera data baserat på datum.

# Hur man gör

För att jämföra två datum i PHP, använd datumfunktionen `strtotime()` som konverterar ett datum till Unix-timestamp. Sedan kan du använda de vanliga jämförelseoperatorerna som `<`, `>`, `<=` och `>=` för att jämföra de två datum.

```PHP
$datum1 = strtotime("2020-01-01");
$datum2 = strtotime("2020-01-15");

if ($datum1 < $datum2) {
    echo "Datum 1 är tidigare än datum 2";
} else {
    echo "Datum 1 är senare än datum 2";
}
```

Output:
```
Datum 1 är tidigare än datum 2
```

# Djupdykning

När man jämför två datum i PHP är det viktigt att förstå att Unix-timestamp är baserat på antalet sekunder som gått sedan 1 januari 1970. Detta innebär att jämförelsen endast kommer att fungera om båda datumen är i samma tidszon. Om datumen är i olika tidszoner måste du konvertera dem till samma tidszon innan jämförelsen.

Det är också viktigt att känna till de olika formaten för datum i PHP, såsom ISO 8601-formatet (`YYYY-MM-DD`) och Unix-timestamp (antal sekunder).

# Se även

För mer information om datum och tid i PHP, se följande länkar:

- [PHP Datum och Tid manual](https://www.php.net/manual/en/function.date.php)
- [PHP Tidszoner manual](https://www.php.net/manual/en/datetime.formats.timezone.php)
- [ISO 8601 specification](https://www.iso.org/iso-8601-date-and-time-format.html)