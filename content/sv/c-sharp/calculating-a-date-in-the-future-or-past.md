---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "C#: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att beräkna ett datum i framtiden eller förflutet innebär att hitta ett datum som är före eller efter ett givet datum med ett visst antal dagar, månader eller år. Exempel på detta kan vara att planera ett möte eller födelsedag eller att skapa ett schema för en period framåt. Det är vanligtvis en funktion som programmerare behöver använda för att göra program mer dynamiska och anpassningsbara.

## Hur man gör:
Det finns flera sätt att beräkna ett datum i framtiden eller förflutet i C#. Här är några olika metoder för att hjälpa dig komma igång:

### Metod 1: Använda DateTime.Add metoden
```C#
DateTime start = new DateTime(2021, 12, 31);
DateTime result = start.AddYears(5).AddMonths(3).AddDays(10);
Console.WriteLine(result); // Output: 2027-04-10
```
I det här exemplet har vi först skapat ett DateTime-objekt med startdatumet 2021-12-31. Sedan använder vi AddYears, AddMonths och AddDays metoder för att lägga till 5 år, 3 månader och 10 dagar till vårt startdatum. Resultatet blir 2027-04-10.

### Metod 2: Använda DateTime.AddDays metoden
```C#
DateTime start = new DateTime(2021, 12, 31);
DateTime result = start.AddDays(50);
Console.WriteLine(result); // Output: 2021-12-31
```
I det här exemplet använder vi bara AddDays metoden för att lägga till 50 dagar till vårt startdatum. Eftersom 2021 inte är ett skottår är det ingen skillnad mellan det ursprungliga datumet och det nya datumet.

## Djupdyka:
Det finns flera andra sätt att beräkna datum i framtiden eller förflutet i C#, som att använda DateTime.Subtract metoden eller DateTime constructor med år, månad och dag argument. Det kan också vara användbart att arbeta med DateTimeOffest-objekt, speciellt om du behöver ta hänsyn till tidszoner.

När man beräknar datum är det också viktigt att hålla koll på skottår, speciellt om man arbetar med stora tidsperioder. Det kan också vara användbart att konvertera resultatet till det lokala datumformatet med DateTime.ToString metoden.

## Se även:
- Microsoft dokumentation för DateTime.Add metoden: https://docs.microsoft.com/en-us/dotnet/api/system.datetime.add?view=net-5.0
- Tutorialspoint tutorial om datumberäkningar i C#: https://www.tutorialspoint.com/getting-different-types-of-dates-in-csharp-programming