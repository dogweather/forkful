---
title:                "C#: Extrahera delsträngar"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att extrahera substrängar från en sträng kan vara en användbar teknik i många C# program. Det kan göra det enklare att manipulera data, hitta specifika delar av en sträng och utföra regelbundna uttryck.

## Hur man gör

För att extrahera substrängar från en sträng i C# kan du använda metoden `Substring()` som finns inbyggd i strängklassen. Denna metod tar två parametrar: en startposition och en längd. Här är ett enkelt exempel på hur du kan använda `Substring()` för att extrahera en del av en sträng:

```C#
string namn = "Jag heter Maria";
string förnamn = namn.Substring(4, 5);
Console.WriteLine(förnamn);
```

I detta exempel kommer utskriften att bli "heter", eftersom det är den del av strängen som börjar på position 4 och är 5 tecken lång.

Du kan också använda `Substring()` tillsammans med andra C# strängmetoder, som `IndexOf()` och `LastIndexOf()`, för att hitta specifika delar av en sträng och sedan extrahera dem. Här är ett exempel på hur du kan göra för att extrahera ett efternamn från en sträng:

```C#
string namn = "Jag heter Maria Andersson";
string efternamn = namn.Substring(namn.IndexOf(" ") + 1, namn.Length - namn.IndexOf(" ") - 1);
Console.WriteLine(efternamn);
```

I detta exempel kommer utskriften att bli "Andersson", eftersom vi först använder `IndexOf()` för att hitta positionen för det första mellanslaget i strängen. Vi lägger sedan till 1 till denna position för att hoppa över mellanslaget och använder `Length`-egenskapen för att få resten av strängen.

## Djupt dyk

När du extraherar substrängar måste du vara försiktig med indexering eftersom den första tecknet i en sträng är index 0, inte 1. Om du anger en längd som är längre än det som finns kvar i strängen kan du få en `ArgumentOutOfRangeException` när du kör din kod.

Det finns också olika sätt att hantera teckenkodning, särskilt vid extrahering av substrängar från flerspråkiga strängar. Du kan behöva använda `Encoding`-klassen för att justera teckenkodningen innan du extraherar dina substrängar.

## Se även

- [Microsoft- dokumentation om Substring-metoden](https://docs.microsoft.com/en-US/dotnet/api/system.string.substring?view=netframework-4.8)
- [C#- String-klass dokumentation](https://docs.microsoft.com/en-US/dotnet/api/system.string?view=netframework-4.8)
- [Tutorial om C#-strängmetoder](https://www.tutorialspoint.com/csharp/csharp_strings.htm)