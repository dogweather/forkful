---
title:                "C#: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett visst mönster är en vanlig åtgärd inom programmering. Det kan vara användbart för att rensa data eller för att manipulera strängar på ett specifikt sätt.

## Så här gör du

För att ta bort tecken som matchar ett mönster i C# kan du använda metoden `Regex.Replace()`. Här är ett enkelt exempel på hur du kan använda den:

```C#
string text = "Bananer är gula men blåbär är blå.";
string mönster = "[blå|gul]a";
string resultat = Regex.Replace(text, mönster, "");
Console.WriteLine(resultat);
```

I detta exempel använder vi ett reguljärt uttryck för att ta bort alla ord som slutar på "a" och har antingen "blå" eller "gul" i början. Output blir: "är men är".

Du kan också använda `Regex.Replace()` för att ta bort alla tecken som inte matchar ett visst mönster. Här är ett exempel på hur du kan göra det:

```C#
string text = "12345abcde";
string mönster = "[^1-5]+";
string resultat = Regex.Replace(text, mönster, "");
Console.WriteLine(resultat);
```

I detta exempel tar vi bort alla tecken som inte är siffror mellan 1 och 5. Output blir: "12345".

## Djupdykning

För att förstå hur `Regex.Replace()` fungerar är det viktigt att förstå reguljära uttryck. Ett reguljärt uttryck (eller regex) är en sträng som beskriver ett mönster som vi vill matcha. Genom att kombinera tecken och speciella teckenkoder kan vi skapa komplexa mönster som hjälper oss att manipulera strängar.

För att lära dig mer om reguljära uttryck och `Regex.Replace()`-metoden, kan du kolla in följande resurser:

- [Regex Tutorial (på svenska)](https://codecademy.com/learn/learn-regex)
- [C# Regex Klass (på svenska)](https://docs.microsoft.com/sv-se/dotnet/standard/base-types/regular-expression-language-quick-reference)

## Se även

- [C# String Manipulation (på svenska)](https://www.java.com/sv/download/faq/programming.shtml)
- [Regex Cheat Sheet (på svenska)](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [RegExr - Online Regex Tester (på svenska)](https://regexr.com/)