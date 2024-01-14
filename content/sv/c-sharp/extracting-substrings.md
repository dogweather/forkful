---
title:    "C#: Extrahera substrängar"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Extrahering av substrängar är en vanlig uppgift inom programmering, oavsett vilket språk man använder. Det är en viktig färdighet som kan hjälpa dig att manipulera och hantera strängar på ett effektivt sätt.

## Så här

Att extrahera substrängar innebär helt enkelt att välja en del av en befintlig sträng. I C# kan vi använda metoden `Substring()` för att åstadkomma detta.

Låt oss säga att vi har en sträng som heter `myString` och innehåller följande: "Hej alla tillsammans!". Om vi vill extrahera ordet "tillsammans" från denna sträng, kan vi göra så här:

```C#
string myString = "Hej alla tillsammans!";
string substring = myString.Substring(9, 10);
Console.WriteLine(substring); // output: tillsammans
```

I det här exemplet använder vi `Substring()`-metoden på vår ursprungliga sträng `myString` och anger två parametrar - startindex och längden på den önskade substrängen. Eftersom ordet "tillsammans" börjar på index 9 och är 10 tecken långt, väljer vi dessa värden för våra parametrar.

Det är också värt att nämna att index i C# börjar på 0, vilket betyder att det första tecknet i en sträng har index 0. Så om vi ville extrahera ordet "Hej" från vår ursprungliga sträng, skulle vi använda parametrarna (0, 3).

## Djupdykning

När det gäller `Substring()`-metoden finns det några saker att tänka på. För det första, om vi bara anger ett startindex som parameter, kommer metoden att extrahera resten av strängen från och med det angivna indexet. Så om vi ville extrahera allt efter "alla" i vår ursprungliga sträng, skulle vi använda `(7)` som parameter.

För det andra, om vi anger ett startindex som är större än längden på vår sträng kommer metoden att returnera ett undantag. Så se till att alltid kolla längden på din sträng innan du extraherar en substräng.

Slutligen är det värt att notera att `Substring()`-metoden inte ändrar den ursprungliga strängen, utan returnerar en ny sträng. Så om du vill ändra en del av en sträng, t.ex. ersätta ett ord, måste du använda metoder som `Replace()` istället.

## Se även

- [C#-strängar - Microsoft Dokumentation](https://docs.microsoft.com/sv-se/dotnet/csharp/programming-guide/strings/)
- [C#-substrings - C# Corner](https://www.c-sharpcorner.com/article/substrings-in-C-Sharp/)