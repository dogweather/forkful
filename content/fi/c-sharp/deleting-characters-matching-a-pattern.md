---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Poistamme merkit, jotka vastaavat tiettyä kaavaa ohjelmoinnissa, jotta voimme tehokkaasti siivota tai muokata tietoja. Se voi auttaa meitä tasaamaan syöttötiedot, poistamaan ei-toivotut elementit tai suorittamaan tehtäviä, kuten merkkijonojen manipulointia.

# Miten:
Tässä on yksinkertainen esimerkki siitä, miten voit poistaa kaikki numerot merkkijonosta käyttämällä Regex.Replace C# monikäyttöistä metodia.
```C#
using System.Text.RegularExpressions;

string input = "Koodaaja123";
string pattern = @"\d+";
string output = Regex.Replace(input, pattern, "");
Console.WriteLine(output);
```
Tulostus näyttäisi seuraavalta:
```C#
"Koodaaja"
```

# Syvä Sukellus
(1) Historiallinen Konteksti: Regex, tai säännölliset lausekkeet, koodauskäytäntö on ollut olemassa jo vuodesta 1951 lähtien. Se alkoi komentorivikäytössä UNIX-järjestelmissä ja on sittemmin sisällytetty moniin eri ohjelmointikieliin, C# mukaan lukien.
(2) Vaihtoehdot: Voit käyttää myös `String.Replace()` -menetelmää, jos kaava ei ole tarpeen tai jos haluat vain korvata yhteneväisen alimerkkijonon. Saatat myös luottaa LINQ-toimintoihin tiettyjen merkkien poistamiseen.
(3) Toteutuksen Tiedot: C# Regex.Replace hyväksyy merkkijonoinputin, säännöllisen lausekkeen `pattern` ja korvaavan merkkijonon. Regex-engine etsii kaikki inputista löytyvät kaavaa vastaavat osiot ja korvaa ne tarjotulla merkkijonolla, tässä tapauksessa tyhjällä merkkijonolla.

# Katso Myös
* Microsoft C# Ohjeet: [Regex.Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
* [Säännölliset lausekkeet C#] (https://www.tutorialsteacher.com/csharp/csharp-regex)
* [C# String.Replace Method](https://www.dotnetperls.com/replace)
* [LINQ:n käyttö C#] (https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/concepts/linq/)