---
title:    "C#: Omvandla en sträng till gemener"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en sträng (string) till små bokstäver (lower case) är en viktig del av programmering. Genom att göra detta kan man enkelt jämföra strängar oberoende av strängarnas storlekar. Detta är också användbart när man vill visa konsistent utdata till användare, oavsett om de har skrivit in sin input med stora eller små bokstäver. Så varför det är viktigt att kunna konvertera en sträng till lower case, låt oss nu prata om hur man faktiskt gör det.

## Så här gör du

För att konvertera en sträng till lower case i C# så finns det inbyggda metoder som man kan använda. Den vanligaste metoden är `ToLower()`, som finns på alla strängar. Här är ett exempel på hur man använder `ToLower()` för att konvertera en sträng till lower case:

```C#
string myString = "HELLO WORLD";
string lowerCaseString = myString.ToLower();
Console.WriteLine(lowerCaseString);

// Output: hello world
```

I detta exempel konverterar vi den ursprungliga strängen "HELLO WORLD" till lower case genom att använda `ToLower()`-metoden och sedan skriva ut den nya strängen till konsolen.

En annan metod som man kan använda är `CultureInfo.InvariantCulture.TextInfo.ToLower()`, som är användbar när man hanterar specifika kulturer och språk. Här är ett exempel på hur man skulle använda denna metod för att konvertera en sträng till lower case:

```C#
string myString = "HeLlO WoRlD";
CultureInfo cultureInfo = new CultureInfo("en-US");
string lowerCaseString = cultureInfo.TextInfo.ToLower(myString);
Console.WriteLine(lowerCaseString);

// Output: hello world
```

Som ni kan se så ger båda metoderna samma resultat, men i det andra exemplet använder vi en specifik `CultureInfo` för att säkerställa att alla bokstäver konverteras på rätt sätt i en engelsk kontext.

## Djupdykning

Nu när vi vet hur vi konverterar en sträng till lower case, låt oss ta en titt på den tekniska aspekten av hur detta faktiskt fungerar. I bakgrunden så finns det en klass som heter `System.Globalization.TextInfo` som innehåller metoder för t.ex. att ändra teckensnitt, konvertera bokstäver och mycket mer. Detta är den klass som används när vi använder `CultureInfo.TextInfo.ToLower()`.

För att förstå hur `ToLower()` fungerar, så måste vi först förstå att alla bokstäver i C# representeras av Unicode-tecken. Varje enskilt Unicode-tecken har en numerisk "kodpunkt" som motsvarar dess position i den globala Unicode-tabellen. I enkla termer så är `ToLower()`-metoden bara en lista över alla bokstäver och deras motsvarande små bokstavs koder i Unicode. När en sträng passerar genom `ToLower()`, så går algoritmen igenom varje tecken i strängen och byter ut det med dess motsvarande små bokstavs kod och returnerar sedan den nya strängen.

## Se även

- [MSDN - string.ToLower() Metod (System)](https://docs.microsoft.com/sv-se/dotnet/api/system.string.tolower?view=net-5.0)
- [MSDN - CultureInfo Class (System.Globalization)](https://docs.microsoft.com/sv-se/dotnet/api/system.globalization.cultureinfo?view=net-5.0)
- [MSDN - TextInfo Class (System.Globalization)](https://docs.microsoft.com/sv-se/dotnet/api/system.globalization.textinfo?view=net-5.0)
- [Unicode-codes](https://unicode-table.com/sv/)