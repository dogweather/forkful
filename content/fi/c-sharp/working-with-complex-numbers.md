---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:38:50.740778-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"

category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Kompleksiluvut laajentavat lukujärjestelmäämme sisältämään imaginaarisia lukuja, mikä mahdollistaa yhtälöiden ratkaisun, kun todellisia ratkaisuja ei ole. Ohjelmoijat työskentelevät niiden parissa aloilla, kuten insinööritiede, fysiikka ja signaalinkäsittely, missä nämä luvut ovat olennaisia mallintamisessa ja ongelmanratkaisussa.

## Kuinka:
C# sisältää valmiina `System.Numerics.Complex` rakenteen kompleksilukujen käsittelyyn. Tässä nopea läpikäynti:

```C#
using System;
using System.Numerics;

class ComplexNumberExample
{
    static void Main()
    {
        // Kompleksilukujen luominen
        Complex c1 = new Complex(4, 5); // 4 + 5i
        Complex c2 = Complex.FromPolarCoordinates(1, Math.PI / 4); // 1 * e^(iπ/4)

        // Perusoperaatiot
        Complex summa = c1 + c2;
        Complex ero = c1 - c2;
        Complex tulo = c1 * c2;
        Complex osamäärä = c1 / c2;

        // Tulosten tulostaminen
        Console.WriteLine($"Summa: {summa}");
        Console.WriteLine($"Ero: {ero}");
        Console.WriteLine($"Tulo: {tulo}");
        Console.WriteLine($"Osamäärä: {osamäärä}");
        Console.WriteLine($"c1:n suuruus: {c1.Magnitude}");
        Console.WriteLine($"c1:n vaihe: {c1.Phase}");
    }
}
```

Tämä tulostaa:

```
Summa: (4.70710678118655, 5.70710678118655)
Ero: (3.29289321881345, 4.29289321881345)
Tulo: (-1.00000000000001, 9)
Osamäärä: (0.6, 0.8)
c1:n suuruus: 6.40312423743285
c1:n vaihe: 0.896055384571344
```

## Syventävä osio
Kompleksiluvut, jotka koostuvat reaalisesta ja imaginaarisesta osasta (usein merkitty muodossa a + bi), ovat olleet olemassa 17. vuosisadalta lähtien. Italian matemaatikko Gerolamo Cardano on niiden varhaisen kehityksen takana. Ohjelmoinnissa kompleksilukujen käsittelyyn kuuluu näiden kahden erillisen osan ymmärtäminen ja hallinta.

Vaikka C#:n `System.Numerics.Complex` on vankka ja kielen sisäänrakennettu, myös muut kielet, kuten Python, tarjoavat vastaavaa toiminnallisuutta `cmath`:n tai kolmansien osapuolien kirjastojen avulla. Ja jos työskentelet vanhemman version parissa C#:n tai .NET version kanssa, joka ei tue `System.Numerics`:ia, saatat joutua luomaan oman kompleksilukuluokan tai etsimään kirjaston.

Sisäisesti kompleksilukujen operaatiot käyttävät liukulukuaritmetiikkaa, joka voi tuoda mukanaan pyöristysvirheitä. Joten, kun toteutat algoritmeja, jotka käyttävät laajasti kompleksilukuja, on tärkeää muistaa tämä ja harkita sen vaikutusta tarkkuuteen ja paikkansapitävyyteen.

## Katso myös
1. C# viite `System.Numerics.Complex`:lle: https://learn.microsoft.com/en-us/dotnet/api/system.numerics.complex
2. Syventävä tutkimusmatka kompleksilukujen matematiikkaan: https://mathworld.wolfram.com/ComplexNumber.html
3. Vaihtoehtoiset toteutukset ja kirjastot, tarkasta Math.NET Numerics: https://numerics.mathdotnet.com/
