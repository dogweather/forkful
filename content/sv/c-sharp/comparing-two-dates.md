---
title:                "C#: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför 

Att jämföra två datum kan vara en viktig del av programmering, speciellt när det gäller att hantera och organisera data. Genom att jämföra två datum kan du få en bättre förståelse för hur dina data har förändrats över tid och identifiera mönster och trender.

## Så här gör du 

Det finns flera sätt att jämföra två datum i C# beroende på dina specifika behov. Här är två exempel på hur du kan göra det:

#### Exempel 1 - Jämföra datumobjekt

För att jämföra två datum objekt kan du använda metoden `Compare()` som finns i `DateTime` klassen. I följande kodexempel använder vi denna metod för att jämföra två datum och returnera en integer som indikerar om det första datumet är före, lika med eller efter det andra datumet.

```C#
DateTime date1 = new DateTime(2020, 04, 20);
DateTime date2 = new DateTime(2020, 04, 27);

int result = date1.Compare(date2);

Console.WriteLine("Date 1 är {0} än Date 2", result > 0 ? "senare" : result < 0 ? "tidigare" : "samma");
// Output: Date 1 är tidigare än Date 2
```

#### Exempel 2 - Jämföra datumsträngar

Om du har datum som lagras som strängar kan du använda `DateTime.Parse()` för att konvertera dem till `DateTime` objekt och sedan jämföra dem. I detta exempel använder vi också `ToString()` metoden för att formatera utskriften.

```C#
string date1 = "2020-04-20";
string date2 = "2020-04-27";

DateTime convertedDate1 = DateTime.Parse(date1);
DateTime convertedDate2 = DateTime.Parse(date2);

Console.WriteLine("Date 1 är {0} än Date 2", convertedDate1 > convertedDate2 ? "senare" : convertedDate1 < convertedDate2 ? "tidigare" : "samma");
// Output: Date 1 är tidigare än Date 2

Console.WriteLine("Date 1 är formaterat som: {0}", convertedDate1.ToString("ddd, d MMM yyyy"));
// Output: Date 1 är formaterat som: Mon, 20 Apr 2020
```

## Djupdykning 

När du jämför två datum är det också viktigt att vara medveten om tidzoner och dagssparande tid. Om du inte specificerar tidszon kommer standardtiden att användas, vilket kan leda till felaktiga jämförelser. En annan viktig faktor är att vissa datummetoder förändrar inte faktiskt datumet som jämförs, utan returnerar istället en kopia med ändringar. Se till att du förstår dessa skillnader för att undvika buggar i din kod.

## Se även 

- [DateTime.CompareTo() metod](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compareto)
- [DateTime.Parse() metod](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse)