---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus tarkoittaa ohjelmakoodin toimivuuden varmistamista automaattisilla testitapauksilla. Koodin testaaminen auttaa löytämään bugeja aikaisessa vaiheessa ja ylläpitämään koodin laatua pitkällä aikavälillä.

## How to:
Käytetään NUnit-testikehystä.

```C#
using NUnit.Framework;

namespace ExampleTests
{
    public class CalculatorTests
    {
        [Test]
        public void Add_TwoNumbers_ReturnsSum()
        {
            // Setup
            var calculator = new Calculator();
            
            // Act
            var result = calculator.Add(5, 7);
            
            // Assert
            Assert.AreEqual(12, result);
        }
    }
    
    public class Calculator
    {
        public int Add(int a, int b)
        {
            return a + b;
        }
    }
}
```

Ajettu testi tuottaa seuraavan tulosteen:

```plaintext
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.235 seconds
```

## Deep Dive
Automatisoidut testit ovat olleet ohjelmistokehityksessä vakiintunut käytäntö jo vuosikymmeniä. Vaihtoehtoja NUnitille ovat esim. MSTest ja xUnit.net. Nämä työkalut seuraavat usein xUnit-arkkitehtuurimallia, missä testit määritellään menetelminä ja ajetaan erillisellä testaustyökalulla.

## See Also
- NUnit: [https://nunit.org/](https://nunit.org/)
- Microsoft's Unit Testing documentation: [https://docs.microsoft.com/en-us/dotnet/core/testing/](https://docs.microsoft.com/en-us/dotnet/core/testing/)
- xUnit.net: [https://xunit.net/](https://xunit.net/)
