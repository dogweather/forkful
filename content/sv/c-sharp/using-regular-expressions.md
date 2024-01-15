---
title:                "Användning av reguljära uttryck"
html_title:           "C#: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Att använda reguljära uttryck är ett kraftfullt verktyg inom programmering som gör det möjligt att söka och manipulera textsträngar på ett flexibelt sätt. Genom att lära sig hur man använder reguljära uttryck kan man effektivisera sitt skriptande och skriva mer robusta program.

## Så här gör du

För att använda reguljära uttryck i C# behöver du använda klassen `Regex` som finns i .NET Framework. Här är ett exempel på hur man kan använda reguljära uttryck för att hitta och ersätta alla förekomster av ett visst ord i en textsträng:

```
using System;
using System.Text.RegularExpressions;

string text = "Det var en gång en katt som hette Nisse";
string nyText = Regex.Replace(text, "katt", "hund");

```

I detta exempel använder vi `Regex.Replace()`-metoden för att ersätta alla förekomster av ordet "katt" med ordet "hund". Nu blir `nyText`-variabeln "Det var en gång en hund som hette Nisse". Här ser du ett annat exempel där vi använder reguljära uttryck för att hitta och extrahera ett visst mönster från en textsträng:

```
using System;
using System.Text.RegularExpressions;

string text = "Min e-postadress är example@example.com";
string mönster = @"[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}";
var match = Regex.Match(text, mönster, RegexOptions.IgnoreCase);

Console.WriteLine(match.Value); // Output: example@example.com
```

I detta exempel använder vi `Regex.Match()`-metoden för att hitta en e-postadress i texten och extrahera den. Det reguljära uttrycket som vi använder följer ett vanligt mönster för att hitta e-postadresser och `Ignorecase`-flaggan gör att det inte spelar någon roll om bokstäverna är stora eller små.

## Djupdykning

Reguljära uttryck kan verka krångligt när man ser dem för första gången, men det finns många hjälpverktyg som kan underlätta användningen av dem. Till exempel finns det online-verktyg som kan hjälpa dig att bygga upp ditt reguljära uttryck steg för steg och testa det mot olika textsträngar för att se om det fungerar som det ska. Några populära verktyg är Regex101 och RegExr.

Ett annat användbart verktyg är Regex Cheat Sheet som ger dig en översikt över de vanligaste tecknen som används i reguljära uttryck och vad de betyder.

Det kan även vara bra att veta att det finns några vanliga fallgropar när man använder reguljära uttryck. Till exempel om man glömmer att inkludera ett "escape"-tecken innan ett speciellt tecken som exempelvis en punkt, vilket kan leda till att man får oönskade resultat. Det kan också vara svårt att hitta balansen mellan att göra uttrycket tillräckligt flexibelt för att hitta olika mönster, samtidigt som det blir för komplext och tar för lång tid att utvärdera.

## Se även

[Forskning om reguljära uttryck](https://arxiv.org/pdf/1208.1739.pdf)

[.NET Framework dokumentation om Regex-klassen](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netframework-4.8)

[Regex Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/pcre)