---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
simple_title:         "Säännöllisten lausekkeiden käyttö"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)

Regular expressions eli säännölliset lausekkeet ovat kuvioita tekstinhakuun ja -käsittelyyn. Ne auttavat löytämään, korvaamaan tai tarkistamaan tekstiä nopeasti ja monipuolisesti.

## How to: (Kuinka tehdä:)

```C#
using System;
using System.Text.RegularExpressions;

class RegexDemo
{
    static void Main()
    {
        // Etsi kaikki sähköpostiosoitteet
        string teksti = "Otto on otto@example.com, Liisa on liisa99@anothermail.fi";
        string pattern = @"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b";

        MatchCollection matches = Regex.Matches(teksti, pattern);

        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }

        // Output:
        // otto@example.com
        // liisa99@anothermail.fi
    }
}
```

## Deep Dive (Sukellus syvemmälle):

Säännölliset lausekkeet juontavat juurensa matemaatikko Stephen Kleenen työhön 1950-luvulla. Vaihtoehtoja säännöllisille lausekkeille ovat mm. string-menetelmät (esim. `Contains`, `IndexOf`) ja parserit, mutta ne eivät yltä regexien monipuolisuuteen. Regexeissä on monia toteutustapoja, C# käyttää .NETin Regex-luokkaa, joka on tehokas ja monipuolinen.

## See Also (Katso myös):

- Microsoftin ohjeet regexeille: [docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- Regex101, harjoittele ja testaa regex-kuvioita: [regex101.com](https://regex101.com/)
- Regexr, opettele regexien käyttöä: [regexr.com](https://regexr.com/)
