---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:52:05.835018-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä & Miksi?
Debug-tulosteiden printtaaminen on koodin tulostamista, nähdäksemme mitä siellä tapahtuu. Käytämme sitä virheiden jäljittämiseen ja ohjelman toiminnan ymmärtämiseen.

## How to:
Miten:
```C#
using System;

class DebugExample {
    static void Main() {
        string message = "Hei, tässä debug-viesti!";
        Console.WriteLine(message); // Tulostaa viestin konsoliin

        int luku = 42;
        Console.WriteLine($"Muuttujan 'luku' arvo on: {luku}"); // Muodostettu tuloste
    }
}
```
Sample output:
```
Hei, tässä debug-viesti!
Muuttujan 'luku' arvo on: 42
```

## Deep Dive
Syvä sukellus:
Printtauksen historia juontuu laitteista, jotka oikeasti printtasivat paperille. Nykyään `Console.WriteLine` on standardi C#-metodi debug-tulosteille. Vaihtoehtoina on käyttää logikirjastoja, kuten NLog tai log4net, jotka tuovat lisää hallintaa. Implementoinnissa on hyvä muistaa, että tuotantokoodista tulisi poistaa debug-tulosteet suorituskyvyn ja turvallisuuden vuoksi.

## See Also
Katso myös:
- Microsoft Docs – Console.WriteLine: https://docs.microsoft.com/dotnet/api/system.console.writeline
- NLog: https://nlog-project.org/
- log4net: https://logging.apache.org/log4net/
