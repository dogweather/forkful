---
title:                "PHP: Räkna ut ett datum i framtiden eller i det förflutna"
simple_title:         "Räkna ut ett datum i framtiden eller i det förflutna"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Det finns många tillfällen i programmering där man behöver beräkna ett datum i framtiden eller förfluten tid. Det kan vara för att skapa en schemaläggning, för att hantera åldersberäkningar eller för att planera för leveranstider. Oavsett anledning är det viktigt att veta hur man korrekt beräknar dessa datum för att undvika fel och missförstånd.

## Så här gör du

För att beräkna ett datum i framtiden eller förfluten tid i PHP finns det flera inbyggda funktioner som kan användas. En av de mest användbara är `strtotime()` som konverterar en sträng till en tidsstämpel. Här är ett exempel på hur man skulle använda den för att beräkna ett datum 10 dagar framåt i tiden:

```PHP
$today = date("Y-m-d"); // nuvarande datum som en sträng
$future_date = strtotime("+10 days", strtotime($today)); // konverterar $today till tidsstämpel och lägger till 10 dagar
echo date("Y-m-d", $future_date); // skriver ut det framtida datumet som en sträng
```

Det finns också många andra användbara funktioner som `mktime()` för att skapa en tidsstämpel baserat på specifika datum och `date_diff()` för att beräkna skillnaden mellan två datum. Det är viktigt att läsa på PHP:s dokumentation för att få en bättre förståelse för hur dessa funktioner kan användas i olika scenarion.

## Djupdykning

När det kommer till att räkna ut datum i framtiden eller förfluten tid är det viktigt att förstå hur tidsstämplar fungerar i PHP. En tidsstämpel är en numerisk representation av datum och tid som börjar vid midnatt (00:00:00) den 1 januari 1970 GMT. Det betyder att alla funktioner som arbetar med tidsstämplar utgår från denna utgångspunkt och kan ge oväntade resultat om man inte är medveten om det.

Det är också viktigt att vara noggrann med vilket format datumet har när man använder `strtotime()`. Om man till exempel skriver in ett datum som är i ett annat format än standardformatet för det land man befinner sig i kan det resultera i felaktiga beräkningar. Därför är det viktigt att alltid använda rätt format när man hanterar datum i PHP, och omvandla dem till standardformatet innan man utför beräkningar.

## Se även

- [PHP Date och Time Funktioner](https://www.php.net/manual/en/ref.datetime.php)
- [PHP Date Formateringsfunktioner](https://www.php.net/manual/en/function.date.php)
- [PHP Strtotime Funktionen](https://www.php.net/manual/en/function.strtotime.php)