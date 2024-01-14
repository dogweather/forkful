---
title:    "C#: Extrahera substrings"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Varför

 I denna bloggpost kommer vi att utforska hur man extraherar substrängar i C# och varför denna teknik kan vara användbar för ditt programmeringsprojekt. Genom att ta en djupdykning i detta koncept hoppas vi kunna ge dig en bättre förståelse för hur substrängar fungerar och hur du kan använda dem i din kod.

## Hur man gör

För att extrahera en substräng från en befintlig sträng i C# kan du använda metoden `Substring()`. Denna metod tar två parametrar: en startposition och en längd. I följande kodexempel kommer vi att använda en sträng som innehåller en mobilnummer och extrahera de sista fyra siffrorna som en separat substräng.

```C#
string mobilnummer = "07XXXXXXXX";
string delSträng = mobilnummer.Substring(mobilnummer.Length - 4, 4);

Console.WriteLine(delSträng);
```

Output: `XXXX`

I detta exempel använde vi `Length`-egenskapen på vår ursprungliga sträng för att bestämma var den fjärde sista siffran börjar, och sedan använde vi en längd på 4 för att extrahera de sista fyra siffrorna. Det är viktigt att komma ihåg att indexeringen i C# börjar från 0, så om du vill extrahera de första fyra siffrorna skulle du behöva ange en startposition på 0 istället.

## Djupdykning

Substrängar kan vara mycket användbara när du arbetar med strängar i C#. De låter dig enkelt separera och manipulera delar av en sträng utan att behöva skapa en helt ny sträng. Några andra vanliga scenarier där substrängar kan vara användbara inkluderar att söka igenom en textsträng för ett visst mönster eller tecken, eller att bearbeta användarinput för att få ut specifika värden.

Det är också värt att notera att `Substring()`-metoden returnerar en ny sträng istället för att ändra den ursprungliga strängen. Detta kan vara användbart om du vill ha kvar det ursprungliga värdet av strängen samtidigt som du extraherar en del av den.

## Se även

- [Microsoft C# - Substring()](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=net-5.0)
- [W3Schools - Split Strings](https://www.w3schools.com/cs/cs_stringsplit.asp)
- [GeeksforGeeks - Substring() Method in C#](https://www.geeksforgeeks.org/c-sharp-substring-method/)