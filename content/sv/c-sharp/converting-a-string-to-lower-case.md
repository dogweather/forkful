---
title:    "C#: Omvandla en sträng till gemener"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Varför 

Att konvertera en sträng till små bokstäver kan vara användbart i många olika situationer. Ofta behövs det när man arbetar med användarinmatningar, till exempel när man gör sökningar eller när man jämför strängar. Genom att konvertera alla bokstäver till små bokstäver så undviks eventuella fel som kan uppstå på grund av olika skrivningar.

##Så här gör du

Ett enkelt sätt att konvertera en sträng till små bokstäver i C# är genom att använda metoden `ToLower()` från klassen `String`. Detta gör att alla bokstäver i strängen omvandlas till små bokstäver och en ny sträng med de konverterade bokstäverna returneras.

```c#
string input = "Hejsan!";
string lowerCase = input.ToLower();

Console.WriteLine(lowerCase); // hejsan!
```

Om du däremot endast vill konvertera vissa delar av en sträng kan du använda metoden `Substring()` från klassen `String` för att först ta ut den aktuella delen av strängen och sedan konvertera den till små bokstäver.

```c#
string input = "Hej på dig!";
string subString = input.Substring(4, 6).ToLower(); // p

Console.WriteLine(subString); // på
```

##Djupdykning

När du konverterar en sträng till små bokstäver i C# så används standarden för små bokstäver på det språk som du använder i din kod. Detta innebär att om du till exempel är svensk så kommer alla svenska bokstäver att konverteras till små bokstäver enligt den svenska standarden.

Det finns också andra metoder som kan användas för att konvertera strängar till små bokstäver, till exempel `ToLowerInvariant()` som alltid returnerar en sträng i små bokstäver oavsett språkinställningar.

##Se även

- C# String ToUpper() Metod: [https://www.w3schools.com/cs/cs_ref_string_tolower.asp](https://www.w3schools.com/cs/cs_ref_string_tolower.asp)
- Utforska fler strängmetoder i C#: [https://www.c-sharpcorner.com/article/c-sharp-string-methods-with-examples/](https://www.c-sharpcorner.com/article/c-sharp-string-methods-with-examples/)