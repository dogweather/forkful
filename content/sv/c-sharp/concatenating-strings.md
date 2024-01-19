---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Strängkonkatenering innebär att sammanfoga två eller flera strängar till en enda sträng. Detta används för att skapa mer komplexa strängar/manipulera textdata utifrån enkel data.

# Hur:
För att konkatenera strängar i C# använder vi '+' operatören. Men man kan också använda `StringBuilder` eller `string.Concat` funktionen.

```C#
string fornamn = "Sven";
string efternamn = "Svensson";
string fulltNamn = fornamn + " " + efternamn; // "Sven Svensson"

// Använda StringBuilder
StringBuilder sb = new StringBuilder();
sb.Append(fornamn);
sb.Append(" ");
sb.Append(efternamn);
string fulltNamn2 = sb.ToString(); // "Sven Svensson"

// Använda string.Concat
string fulltNamn3 = string.Concat(fornamn, " ", efternamn); // "Sven Svensson"
```

## Fördjupning
Historiskt sett var `+` operatören den föredragna metoden för att konkatenera strängar i C#, men det kan leda till minnesproblem vid stora operationer eftersom varje `+` operation skapar en ny sträng. Därför introducerades klassen `StringBuilder` och metoden `string.Concat` för mer effektiv konkatenering.

Alternativt till dessa metoder finns också funktionerna `string.Join` och `string.Format` som kan vara mer lämpliga beroende på situation.

```C#
string fulltNamn4 = string.Format("{0} {1}", fornamn, efternamn); // "Sven Svensson"
string fulltNamn5 = string.Join(" ", fornamn, efternamn); // "Sven Svensson"
```

## Se också
För mer information om strängkonkatenering och andra sätt att manipulera strängar i C#, se följande länkar:

- [Strängmanipulation i C#](https://docs.microsoft.com/sv-se/dotnet/csharp/programming-guide/strings/)
- [Använda StringBuilder för effektiv strängkonkatenering](https://docs.microsoft.com/sv-se/dotnet/api/system.text.stringbuilder)
- [String.Format metoden](https://docs.microsoft.com/sv-se/dotnet/api/system.string.format)
- [String.Join metoden](https://docs.microsoft.com/sv-se/dotnet/api/system.string.join)