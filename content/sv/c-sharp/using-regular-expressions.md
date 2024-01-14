---
title:    "C#: Att använda reguljära uttryck"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Regular expressions, eller regex, är ett kraftfullt verktyg för att söka och manipulera strängar i ett program. Genom att använda regex kan du effektivt hitta mönster i texten och sedan utföra åtgärder baserat på dessa mönster. Detta sparar tid och minskar risken för felaktig kodning.

## Så här gör du

För att börja använda regular expressions i C# måste du först inkludera System.Text.RegularExpressions namespace i din kod.

```C#
using System.Text.RegularExpressions;
```

För att använda regex för att testa en sträng, använd Regex.Match metoden tillsammans med ett mönster som specificerar vad du söker efter. Till exempel, om du vill hitta alla förekomster av bokstaven "e" i en sträng, kan du använda följande kod:

```C#
string text = "Hej, jag heter Lisa!";
MatchCollection matchar = Regex.Matches(text, "e");
```

Den här koden kommer att returnera en MatchCollection med alla förekomster av "e" i strängen. Du kan sedan använda denna MatchCollection för att utföra önskad handling, som att ersätta alla förekomster av "e" med en annan bokstav.

För att använda vanliga uttryck för att ersätta en viss del av en sträng, använd Regex.Replace metoden. Till exempel, om du vill byta ut alla siffror i en sträng med stjärnor, kan du använda följande kod:

```C#
string text = "Jag är 26 år gammal.";
string nyText = Regex.Replace(text, "[0-9]", "*");
```

Den här koden kommer att ersätta alla siffror i strängen med "*". Det finns många olika möjligheter med hjälp av regex och du kan skapa avancerade mönster för att matcha specifika strängar.

## Djupdykning

En viktig aspekt av att använda regular expressions är att förstå syntaxen för att skapa mönster. Det finns olika symboler och metakaraktärer som kan hjälpa dig att bygga avancerade mönster. Här är några av de vanligaste symbolerna och deras betydelser:

- "." Matchar en enda bokstav eller siffra.
- "?" Matchar en förekomst av den tidigare symbolen eller ingenting.
- "*" Matchar noll eller flera förekomster av den tidigare symbolen.
- "+" Matchar en eller flera förekomster av den tidigare symbolen.
- "^" Matchar början av en sträng.
- "$" Matchar slutet av en sträng.
- "[]" Matchar en av flera olika tecken inom parentes.
- "[^]" Matchar alla tecken som inte finns inom parentes.
- "\" Escapar en specialtecken så att det kan matchas som en vanlig bokstav.

Det är också viktigt att veta att regex är fallkänsliga, vilket innebär att bokstäver måste matcha exakt för att koden ska fungera som förväntat.

## Se även

- [Microsoft Dokumentation om regular expressions i C#](https://docs.microsoft.com/sv-se/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex Tutorial på svenska](https://regex-tutorial.net/regex-lab-swe/) 
- [Regex Tester för att testa dina mönster](https://regex101.com/)