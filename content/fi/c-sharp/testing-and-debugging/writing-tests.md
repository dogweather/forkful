---
title:                "Testien kirjoittaminen"
aliases:
- /fi/c-sharp/writing-tests.md
date:                  2024-02-03T19:30:40.310019-07:00
model:                 gpt-4-0125-preview
simple_title:         "Testien kirjoittaminen"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?

Testien kirjoittaminen C#:ssa tarkoittaa automatisoitujen skriptien luomista koodisi toiminnallisuuden varmistamiseksi, jotta se käyttäytyy odotetulla tavalla. Ohjelmoijat tekevät sen löytääkseen bugit aikaisin, helpottaakseen koodin uudelleenjärjestelyä ja varmistaakseen, että uudet muutokset eivät riko olemassa olevia toimintoja, mikä lisää ohjelmiston laatua ja luotettavuutta.

## Miten:

C#-kehittäjät käyttävät pääasiassa NUnit- tai xUnit-kehyksiä testien kirjoittamiseen niiden joustavuuden ja laajan ominaisuusjoukon vuoksi. Tässä on perusesimerkki, jossa NUnitia käytetään yksinkertaisen yhteenlaskufunktion testaamiseen:

1. **Asenna NUnit ja NUnit3TestAdapter** NuGet Package Managerin tai .NET CLI:n kautta:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Luo C#-luokkakirjasto**projekti, jos et ole vielä tehnyt niin.

3. **Kirjoita yksinkertainen funktio** testattavaksi. Esimerkiksi yhteenlaskumenetelmä luokassa nimeltä `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Kirjoita testiluokka** käyttäen NUnitia:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **Suorita testi** käyttäen IDE:si testiajo-ohjelmaa tai .NET CLI:tä:
```powershell
dotnet test
```

### Esimerkkituloste:

Olettaen, että testisi läpäisee, sinun pitäisi nähdä tuloste, joka muistuttaa tätä:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Sekuntia
```

### Käyttäen xUnitia:

Jos mieluummin käytät xUnitia, asennus on samankaltainen kuin NUnitilla. Näin kirjoitat testiesimerkin `Calculator`-luokalle käyttäen xUnitia:

1. **Asenna xUnit ja xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Kirjoita testiluokka käyttäen xUnitia**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **Suorita testi käyttäen .NET CLI:tä** tai IDE:si integroitua testiajo-ohjelmaa.

Sekä NUnit että xUnit tarjoavat tehokkaita ominaisuuksia parametrisoituun testaukseen, pystytys/purkutoiminnoihin ja testien järjestämiseen kategorioihin, mikä tekee niistä korvaamattomia työkaluja C#-ohjelmoijan työkalupakkiin koodin laadun ja toiminnallisuuden varmistamiseksi.
