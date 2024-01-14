---
title:                "C#: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering. Det kan spara tid och göra det lättare att hantera stora mängder text. Med hjälp av C# kan du enkelt automatisera denna process och göra ditt arbete mer effektivt.

## Hur man gör

För att söka och ersätta text i C#, kan du använda dig av metoden `Replace` som finns tillgänglig för alla strängar. Denna metod tar in två parametrar: den första är den text som ska ersättas och den andra är ersättningstexten. Om du vill ersätta allt förekomst av en viss sträng i en större textmängd kan du använda dig av `Replace`-metoden på följande sätt:

```C#
string myText = "Hej, mitt namn är Johan";
string replacedText = myText.Replace("Johan", "Anna");
Console.WriteLine(replacedText); // Utskrift: "Hej, mitt namn är Anna"
```

Du kan också inkludera fler parametrar i `Replace`-metoden för att göra sök- och ersättningsprocessen mer specifik. Till exempel kan du ange att du bara vill ersätta texten om den är skriven med stora bokstäver. Du kan också använda dig av reguljära uttryck för att göra mer komplexa sökningar och ersättningar. Nedan finns ett exempel på hur du kan göra en mer specifik sökning och ersättning:

```C#
string myText = "Hej, jag heter Johan och jag gillar att programmera";
string replacedText = myText.Replace("Johan", "Anna", StringComparison.OrdinalIgnoreCase); // Ersätter endast textskiftningar som är skrivna med annan bokstavskombination
Console.WriteLine(replacedText); // Utskrift: "Hej, jag heter Anna och jag gillar att programmera"
```

## Djupdykning

Det finns många olika sätt att använda sig av sök- och ersättningsfunktionen i C# och det är viktigt att förstå hur man kan anpassa dem efter sina behov. En sak som är viktig att notera är att `Replace`-metoden endast returnerar den modifierade strängen och inte ändrar den ursprungliga strängen. Om du vill ändra den ursprungliga strängen måste du tilldela resultatet av `Replace`-metoden till den variabel som innehåller den ursprungliga strängen.

För mer information och möjligheter kring sök- och ersättningsfunktionen i C#, kan du titta närmare på dokumentationen för `Replace`-metoden och utforska olika reguljära uttryck.

## Se även

- [C# dokumentation för Replace-metoden](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [Reguljära uttryck i C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Exempel för sök och ersätt i C#](https://www.c-sharpcorner.com/UploadFile/mahesh/search-and-replace-a-substring-from-a-string-in-C-Sharp/)