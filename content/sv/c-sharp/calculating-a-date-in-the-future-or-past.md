---
title:    "C#: Beräkna ett datum i framtiden eller det förflutna"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller det förflutna är en nyttig färdighet i programmeringsvärlden. Det kan hjälpa dig att planera för framtida händelser eller hantera datum i ditt program. I den här bloggposten kommer vi att utforska hur man kan göra detta i C#.

## Så här gör du
För att beräkna ett datum i framtiden eller det förflutna i C#, behöver vi använda DateTime-klassen. Låt oss först skapa en ny instans av DateTime som representerar dagens datum:

```C#
DateTime today = DateTime.Today;
```

Nu kan vi använda olika metoder från DateTime-klassen för att beräkna ett datum i framtiden eller det förflutna. Till exempel, om vi vill veta vilket datum det är om en månad från idag, kan vi använda AddMonths-metoden:

```C#
DateTime futureDate = today.AddMonths(1);
Console.WriteLine("Om en månad från idag är det: " + futureDate.ToString("d"));
```

Output:
```
Om en månad från idag är det: 08/04/2021
```

På liknande sätt kan vi använda AddDays, AddYears eller andra metoder för att beräkna datum på olika sätt.

För att beräkna ett datum i det förflutna, använder vi bara negativa värden. Till exempel, om vi vill veta vilket datum det var för en vecka sedan, kan vi använda Substract-metoden:

```C#
DateTime pastDate = today.Subtract(new TimeSpan(7,0,0,0));
Console.WriteLine("För en vecka sedan var det: " + pastDate.ToString("d"));

```

Output:
```
För en vecka sedan var det: 07/21/2021
```

## Djupdykning
Det finns flera andra metoder och egenskaper i DateTime-klassen som kan vara användbara för beräkning av datum. Till exempel, om du behöver lägga till ett specifikt antal månader eller dagar till ett datum, kan du använda DateTime.Add metoden och ange antalet månader eller dagar som en parameter. Dessutom kan du använda DateTime.Compare-metoden för att jämföra två datum och se vilket som kommer före det andra.

Det är också viktigt att notera att DateTime-klassen hanterar datum och tider i ditt lokala tidszon. Om du behöver hantera datum och tider i andra tidszoner, kan du använda DateTimeOffset-klassen istället.

## Se även
- [Microsoft Docs: DateTime Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Microsoft Docs: DateTimeOffset Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-5.0)
- [C# DateTime Tutorial](https://www.c-sharpcorner.com/UploadFile/0c1bb2/date-and-time-in-C-Sharp-language/)