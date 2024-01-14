---
title:                "PHP: Beräkna ett datum i framtiden eller förflutna"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför

Att kunna beräkna ett datum i framtiden eller i det förflutna är en användbar funktion inom programmering som kan användas för många olika ändamål. Till exempel kan det vara användbart för att skapa en kalenderapplikation, för att planera resor eller för att hålla koll på åldern på ett visst objekt. Oavsett anledning kan det vara värt att lära sig hur man gör det.

# Så här gör du

För att beräkna ett datum i framtiden eller förflutna i PHP kan du använda funktionen `strtotime`. Den tar in två parametrar, ett startdatum och en längd av tiden som ska läggas till eller subtraheras från startdatumet.

```PHP
//datumet idag
$date = date('Y-m-d');

//beräknar datumet om 10 dagar
$future_date = strtotime('+10 days', $date);

echo date('Y-m-d', $future_date);
//resultat: 2020-02-11

//beräknar datumet om 3 månader tillbaka
$past_date = strtotime('-3 months', $date);

echo date('Y-m-d', $past_date);
//resultat: 2019-11-11
```

Det är viktigt att notera att funktionen `strtotime` returnerar resultatet i sekunder. För att sedan omvandla det till ett datum i rätt format används funktionen `date`. Du kan också använda dig av olika tidsenheter som `weeks`, `years` eller `hours` istället för `days` i `strtotime` beroende på vad du behöver.

# Djupdykning

För att beräkna mer komplexa datum kan du kombinera `strtotime` med andra PHP datumfunktioner, som för exempel `date_diff`, som kan räkna ut skillnaden mellan två datum. Du kan också enkelt jämföra olika datum med hjälp av `strtotime` för att se om de ligger före eller efter varandra.

Det finns också flera olika format du kan använda för ditt datum, som `Y-m-d` som vi använde tidigare, men även `d/m/Y` eller `Y/m/d`. Det är viktigt att du använder rätt format beroende på vad du behöver för ditt projekt.

# Se även

Här är några användbara resurser för att lära dig mer om att beräkna datum i PHP:

- [PHP: strtotime function](https://www.php.net/manual/en/function.strtotime.php)
- [PHP: date function](https://www.php.net/manual/en/function.date.php)
- [PHP: date_diff function](https://www.php.net/manual/en/function.date-diff.php)