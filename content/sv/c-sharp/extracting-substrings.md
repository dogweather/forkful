---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Utvinning av understrängar innebär att ta en specifik sekvens av tecken från en sträng. Programmerare gör detta för att bearbeta eller analysera data mer effektivt. 

## Så här:

Vi kan använda metoden `Substring` i C# för att extrahera en understräng. Så här gör du:

```C#
string str = "Hej Världen!";
string subStr = str.Substring(0, 3);

Console.WriteLine(subStr);
```

Kodbussen ovan skriver ut `Hej`, vilket är den första understrängen i `Hej Världen!`.

```C#
string str = "Hej Världen!";
string subStr = str.Substring(4);

Console.WriteLine(subStr);
```

Kodbussen ovan skriver ut `Världen!`, vilket är understrängen som börjar från den 5:e positionen.

## Djupdykning

Historiskt sett har utvinning av understrängar varit en nödvändig operation i programmering. Det låter oss manipulera och begära data på mer komplexa sätt. 

Alternativt till `Substring` kan du även använda `Split` funktionen om du behöver extrahera flera delar av en sträng på en gång.

När det gäller att implementera substring extrahering i C# så sker det med en tidkomplexitet av O(n), där n är antalet extraherade tecken.

## Se även 

För mer detaljerad information, kolla följande länkar:

- Microsoft's C#-guide om `Substring`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)