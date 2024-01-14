---
title:                "C#: Skriva tester"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att utveckla programvara. Genom att skriva tester kan vi försäkra oss om att vår kod fungerar som tänkt och inte bryter när vi gör ändringar i den. Detta gör det också lättare att hitta och lösa buggar i koden.

## Så här gör du

Skriva tester i C# är enkelt och kan delas in i tre steg: förberedelse, utförande och utvärdering. Först behöver vi förbereda vår testmiljö genom att inkludera nödvändiga bibliotek och instansiera de objekt som behövs för att köra testerna. Sedan skriver vi våra tester i separata metoder och använda assert-satser för att säkerställa att koden uppför sig som förväntat. Slutligen utvärderar vi våra testresultat och ser till att alla tester passerar innan vi skickar in vår kod.

```C#
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ExampleTestProject
{
    [TestClass]
    public class ExampleTests
    {
        [TestMethod]
        public void TestMethod1()
        {
            // Förberedelse
            int a = 5;
            int b = 10;

            // Utförande
            int result = a + b;

            // Utvärdering
            Assert.AreEqual(15, result); // Assert-sats som säkerställer att a + b är lika med 15
        }
    }
}
```

I exemplet ovan ser vi hur vi först förbereder våra variabler, utför en beräkning och sedan utvärderar resultatet genom att använda en Assert-sats. Genom att fortsätta på detta sätt för alla scenarier som vår kod ska hantera, kan vi skapa en bra täckning av tester för vår applikation.

## Djupdykning

Det finns många olika tekniker och verktyg för att skriva tester i C#, såsom NUnit, xUnit och MSTest. Det är också möjligt att skriva enhetstester, integrationstester och systemtester för att täcka olika aspekter av vår kod. En annan viktig aspekt är att testa både positiva och negativa scenarier för att se till att vår kod hanterar alla möjliga fall.

Ett annat tips är att skapa automatiserade tester som kan köras regelbundet för att säkerställa att vår kod fortfarande fungerar efter att vi har gjort ändringar. Detta sparar oss mycket tid och ansträngning i framtiden eftersom vi inte behöver manuellt testa all vår kod varje gång vi gör en förändring.

## Se även

- [Enkla steg för att skriva bra tester i C#](https://medium.com/om-programmering/hur-du-skriver-bra-tester-i-c-5e63f5168623)
- [NUnit dokumentation](https://github.com/nunit/docs/wiki)
- [xUnit dokumentation](https://xunit.net/docs/getting-started/netfx/visual-studio)
- [MSTest dokumentation](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest)