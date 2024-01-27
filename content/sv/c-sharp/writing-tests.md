---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester innebär att du skapar kod som kontrollerar att annan kod fungerar som den ska. Programmerare gör detta för att snabbt kunna upptäcka buggar, garantera kvalitet och förenkla framtida underhåll.

## Hur gör man:
För att skriva tester i C#, kan du använda testramverket NUnit. Kolla koden nedan för ett enkelt test.

```C#
using NUnit.Framework;

namespace MyApplication.Tests
{
    [TestFixture]
    public class ExampleTests
    {
        [Test]
        public void AdditionTest()
        {
            Assert.AreEqual(4, 2 + 2);
        }
    }
}
```

Kör testet och så bör det lyckas. Testresultatet ser ut ungefär så här:

```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.234s
```

## Fördjupning
Tester i programvaruutveckling har funnits sedan 1950-talet. Alternativ till NUnit inkluderar xUnit och MSTest i C#-ekosystemet. Detaljerna vid testskrivning inkluderar att isolera testfall, mocka beroenden och organisera test i logiska samlingar.

## Se även
- NUnit: https://nunit.org/
- xUnit: https://xunit.net/
- MSTest documentation: https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest
