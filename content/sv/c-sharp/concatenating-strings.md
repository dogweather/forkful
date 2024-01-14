---
title:                "C#: Sammanslåning av strängar"
simple_title:         "Sammanslåning av strängar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Varför använda sig av strängkonkatinering?

Ett av de vanligaste sätten att manipulera text i C# är genom att konkatinera, eller sammanfoga, strängar. Detta är användbart för att skapa dynamiska meddelanden, utskrifter eller för att bygga upp SQL-statement.

# Så här använder du strängkonkatinering i C#

För att konkatinera strängar i C#, använder man sig av "+" operatorn mellan två strängar. Till exempel:

```C#
string förnamn = "Anna";
string efternamn = "Andersson";
string fulltNamn = förnamn + ' ' + efternamn;
Console.WriteLine(fulltNamn); //output: Anna Andersson
```

Det är också möjligt att konkatinera flera strängar genom att upprepa "+" operationen. Till exempel:

```C#
string förnamn = "Anna";
string förnamn = "Maria";
string förnamn = "Eva";
string allaNamn = förnamn + ' ' + efternamn;
Console.WriteLine(allaNamn); //output: Anna Maria Eva
```

Man kan också använda sig av placeholders, genom att använda sig av "string.Format()" metoden. Till exempel:

```C#
string förnamn = "Anna";
int ålder = 25;
string meddelande = string.Format("Hej, jag heter {0} och jag är {1} år gammal.", förnamn, ålder);
Console.WriteLine(meddelande); //output: Hej, jag heter Anna och jag är 25 år gammal.
```

# Djupdykning i strängkonkatinering

Vad händer egentligen bakom kulisserna när man konkatinerar strängar? När man använder "+" operatorn på två strängar, så skapar C# en ny sträng som innehåller de två ursprungliga strängarna. Detta innebär att varje gång man använder "+" operatorn, så skapas en helt ny sträng, vilket kan bli ineffektivt om man behöver konkatinera många strängar.

En bättre metod för att konkatinera många strängar är att använda sig av StringBuilder klassen. Den låter dig bygga upp en sträng stegvis, vilket resulterar i bättre prestanda och minnesanvändning. Till exempel:

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Hej, mitt namn är");
sb.Append("Anna");
string res = sb.ToString();
Console.WriteLine(res); //output: Hej, mitt namn är Anna
```

# Se även

- [Microsoft Docs: String.Concat Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=net-5.0)
- [Microsoft Docs: StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)
- [C# String Concatenation and StringBuilder](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/string-concatenation-and-builder)