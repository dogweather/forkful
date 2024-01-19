---
title:                "Ta bort tecken som matchar ett mönster"
html_title:           "Arduino: Ta bort tecken som matchar ett mönster"
simple_title:         "Ta bort tecken som matchar ett mönster"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att radera tecken som matchar ett mönster innebär att man systematiskt tar bort specifika tecken från en sträng baserat på vissa kriterier. Detta gör programmerare för att rensa upp data, förändra strängformat eller filtrera oönskade tecken.

## Hur gör man:

Här är ett exempel på hur man kan radera tecken som matchar ett mönster i C#.

```csharp
using System.Text.RegularExpressions;

public class Program
{
    public static void Main()
    {
        string inputString = "ABC123!@#XYZ789*(&)";
        string pattern = "[0-9!@#*(&)]";
        string outputString = Regex.Replace(inputString, pattern, "");
        System.Console.WriteLine(outputString);
    }
}
```

Det där koden kommer skriva ut "ABCXYZ", eftersom alla siffror och symboler har tagits bort. 

## Djupdykning:

När det gäller historisk kontext uppstod behovet av att radera tecken som matchar ett mönster med utvecklingen av reguljära uttryck, ett kraftfullt verktyg för strängmanipulering.

Ett alternativ till `Regex.Replace` kan vara att använda LINQ för att filtrera ut tecken. Detta kan vara mer läsbart men inte lika effektivt eller kraftfullt.

```csharp
string outputString = new string(inputString.Where(c => !char.IsDigit(c) && !char.IsSymbol(c)).ToArray());
```

Implementationen av övre `Regex.Replace` använder det så kallade .NET Frameworks `Regex`-klassen, som erbjuder en effektiv metode för att matcha strängar mot reguljära uttryck och utföra olika operationer på matchningarna.

## Se också:

1. [Microsofts dokumentation om `Regex.Replace`](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
2. [Microsofts guide till LINQ och hur man använder det för att filtrera strängar](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
3. [Mer information om reguljära uttryck i .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)