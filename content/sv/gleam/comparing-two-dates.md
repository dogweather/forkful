---
title:    "Gleam: Jämförande av två datum"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt är det viktigt att kunna jämföra två datum för att hantera tidsberoende data och göra korrekta beslut. I denna bloggpost kommer vi att utforska hur man kan jämföra datum i Gleam och ge lite djupare information om ämnet.

## Hur Man Gör

För att kunna jämföra två datum i Gleam behöver vi först skapa två instanser av typen `Date`. Vi kan göra detta genom att använda funktionen `make` och skicka in år, månad och dag som argument.

```Gleam
let date_one = Date.make(2021, 5, 1)
let date_two = Date.make(2021, 6, 15)
```

Nu när vi har två datum att jämföra, kan vi använda följande funktioner för att få information om dem:

- `Date.after(date_one, date_two)` - Returnerar true om date_one är efter date_two.
- `Date.before(date_one, date_two)` - Returnerar true om date_one är före date_two.
- `Date.diff_in_days(date_one, date_two)` - Returnerar antalet dagar mellan date_one och date_two.

Låt oss till exempel använda dessa funktioner för att se om ett event sker inom en månad från och med idag:

```Gleam
let today = Date.todays_date()
let event_date = Date.make(2021, 9, 1)

if Date.before(event_date, Date.add_days(today, 30)) {
  io.print("Eventet kommer att ske inom en månad från och med idag!")
} else {
  io.print("Eventet kommer inte att ske inom en månad från och med idag.")
}
```

Output:
```
Eventet kommer att ske inom en månad från och med idag!
```

## Djupdykning

När vi jämför datum i Gleam, är det viktigt att förstå att de alltid kommer att vara lokaliserade baserat på tidszonen som används i systemet. Det betyder att om du jämför två datum som har olika tidszoner, kommer resultatet att vara felaktigt. Det är viktigt att se till att alla datum är i samma tidszon innan du jämför dem.

En annan faktor att tänka på är att Gleam inte har inbyggda funktioner för att hantera tidpunkter och tidsintervall. Därför måste vi använda `Date.add_*`-funktionerna för att göra beräkningar baserat på tiden. Till exempel, för att få antalet dagar mellan två datum, måste vi göra en beräkning baserad på skillnaden mellan de två datumens tidpunkter.

## Se Även

- [Gleam Date Modul Dokumentation](https://gleam.run/documentation/stdlib/date.html)
- [Så här använder du tidszoner i Gleam](https://jordankasper.com/gleam-time-zone-implementation/)
- [Jämförelse av datum i andra programmeringsspråk](https://www.guru99.com/date-compare-operator-php-python-java.html)

## Se Även

- [Gleam Date-modulens dokumentation](https://gleam.run/documentation/stdlib/date.html)
- [Så här använder du tidszoner i Gleam](https://jordankasper.com/gleam-time-zone-implementation/)
- [Jämförelse av datum i andra programmeringsspråk](https://www.guru99.com/date-compare-operator-php-python-java.html)