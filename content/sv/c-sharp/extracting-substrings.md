---
title:                "C#: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
I många tillfällen när du hanterar textsträngar i ditt C#-program behöver du bara en del av strängen, inte hela. Att extrahera substrängar är en användbar teknik för att hantera text av olika längder och format. Det sparar tid och gör koden mer effektiv.

## Hur man
För att extrahera en delsträng från en större sträng kan du använda metoden `Substring` i C#. Det här är ett enkelt exempel som visar hur man extraherar en  delsträng från en längre text:

```C#
string originalText = "Hello from Sweden!";
string extractedText = originalText.Substring(11, 6);
Console.WriteLine(extractedText);
```

Det första argumentet i metoden är indexet för var delsträngen ska börja, och det andra argumentet är längden på delsträngen. I det här fallet börjar substrängen vid index 11 (räknat från 0) och är 6 tecken lång, vilket ger "Sweden" som utmatning. 

## Djupdykning
När du använder `Substring`-metoden är det viktigt att tänka på att indexeringen börjar vid 0 och att längden på substrängen inte kan vara längre än den ursprungliga strängen. Om du försöker använda ett index som är utanför gränserna för strängen kommer du att få ett fel. Till exempel, om du skulle försöka extrahera en delsträng från index 20 i vårt exempel ovan skulle koden generera ett fel.

En annan användbar metod för att extrahera substrängar är `Remove`. Denna metod tar bort en delsträng från en större sträng baserat på ett angivet intervall. Detta kan vara användbart om du vill ta bort ett visst ord eller tecken från en sträng. Här är en exempelkod som visar hur man tar bort ett ord från en sträng:

```C#
string originalText = "Hello from Sweden!";
string removedText = originalText.Remove(11, 6);
Console.WriteLine(removedText);
```

Det första argumentet är startindexet för den delsträng som ska tas bort, och det andra argumentet är längden på den delsträngen. I detta fall tar vi bort "Sweden" från den ursprungliga strängen och får "Hello from!" som utdata.

## Se även
- `Substring`-metoden i C#: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- `Remove`-metoden i C#: https://docs.microsoft.com/en-us/dotnet/api/system.string.remove