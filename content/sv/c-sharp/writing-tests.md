---
title:    "C#: Att skriva tester"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Varför
Att skriva tester är en viktig del av att utveckla kvalitativa program i C#. Tester hjälper till att identifiera och förebygga fel i koden, vilket resulterar i en bättre användarupplevelse och minskar risken för problem i framtiden.

## Hur man gör det
Att skriva tester kan verka som en tidskrävande process, men det finns enkla metoder för att göra det smidigare. Nedan följer ett exempel på hur man kan skriva tester för en metod som summerar två tal:

```
C# public static int Sum(int num1, int num2)
{
    return num1 + num2;
}

C# [Test]
public void TestSum()
{
    int result = Sum(2, 3);
    Assert.AreEqual(5, result);
} 
```

I detta exempel används ett enkelt test med hjälp av NUnit-frameworket. Vi definierar en metod som testar vår `Sum`-metod och ser till att resultatet är korrekt genom att använda `Assert.AreEqual()`.

## Djupdykning
Att skriva tester handlar inte bara om att testa funktionernas resultat, utan också om att täcka alla möjliga fall och scenarion. Om vi återvänder till exemplet ovan, kan vi till exempel testa vad som händer om vi matar in negativa tal eller decimaltal.

Ytterligare ett viktigt koncept inom testning är att skriva testbara koder. Detta innebär att koden ska vara skriven på ett sådant sätt att den är lätt att testa, med avgränsade och återanvändbara funktioner.

## Se även
- [En introduktion till testning i C#](https://teamtreehouse.com/library/intro-to-testing-in-c)
- [Tutorial: Enhetstestning med NUnit](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-nunit)
- [Tips för att skriva bättre tester i C#](https://codingexplained.com/coding/c-sharp/new-to-unit-testing-in-c-sharp-5-tips-to-get-started-today)