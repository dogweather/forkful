---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "PHP: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför
 Vill du lära dig hur man beräknar ett datum i framtiden eller det förflutna? Då har du kommit till rätt plats! Att kunna beräkna datum är en viktig färdighet inom programmering och kan hjälpa dig att bygga mer dynamiska och användbara webbapplikationer.

# Så här gör du
För att beräkna ett datum i framtiden eller det förflutna behöver du först och främst ha kunskap om hur datum och tid representeras i PHP. Datumet lagras vanligtvis som ett heltal (Unix-timestamp) som representerar antalet sekunder som har gått sedan 1 januari 1970 00:00:00 UTC. Med hjälp av PHP Date-funktionen kan du sedan omvandla detta heltal till ett mer läsbart format.

```PHP
// För att beräkna ett datum i framtiden, lägg till antalet sekunder till det aktuella datumet
$nästaVecka = time() + (7 * 24 * 60 * 60); // 7 dagar * 24 timmar * 60 minuter * 60 sekunder
echo date('Y-m-d', $nästaVecka); // Skriver ut datumet för nästa vecka i formatet YYYY-MM-DD
```

```PHP
// För att beräkna ett datum i förflutna, dra av antalet sekunder från det aktuella datumet
$igår = time() - (24 * 60 * 60); // 1 dag * 24 timmar * 60 minuter * 60 sekunder
echo date('Y-m-d', $igår); // Skriver ut datumet för igår i formatet YYYY-MM-DD
```
Output:
```
2021-04-05
```

# Djupdykning
Som du kanske märkte i kodexemplen ovan, används Date-funktionen för att omvandla Unix-timestamp till ett mer läsbart datumformat. Det finns också andra användbara funktioner som kan hjälpa dig att manipulera datum och tid i PHP, till exempel strtotime() och DateTime-klassen.

För att beräkna ett datum i en specifik tidszon kan du ange den som ett valfritt argument till Date-funktionen. Du kan också använda DateInterval-klassen för att lägga till eller dra av tidsintervall från ett datum, som i exemplet nedan där vi lägger till 10 dagar till det aktuella datumet.

```PHP
$datum = new DateTime();
$datum->add(new DateInterval('P10D')); // P för period och 10D för 10 dagar
echo $datum->format('Y-m-d'); // Skriver ut datumet 10 dagar från nu i formatet YYYY-MM-DD
```
Output:
```
2021-04-15
```

# Se även
Här är några användbara länkar för vidare läsning om datum och tidshantering i PHP:
- [PHP Date-funktionen](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime() funktionen](https://www.php.net/manual/en/function.strtotime.php)
- [PHP DateTime-klassen](https://www.php.net/manual/en/class.datetime.php)
- [PHP DateInterval-klassen](https://www.php.net/manual/en/class.dateinterval.php)