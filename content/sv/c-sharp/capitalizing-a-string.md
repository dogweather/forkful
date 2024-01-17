---
title:                "Att göra en sträng med stor bokstav"
html_title:           "C#: Att göra en sträng med stor bokstav"
simple_title:         "Att göra en sträng med stor bokstav"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kapitalisera en sträng betyder att göra den första bokstaven stor i varje ord. Detta är en vanlig konvention inom programmering för att göra koden mer läsbar och förståelig. Det hjälper också till att skilja mellan variabler och funktioner.

## Hur man gör:

För att kapitalisera en sträng i C# kan du använda metoden ```ToUpper()```. Här är ett exempel:

```C#
string str = "hello world";
Console.WriteLine(str.ToUpper());
```

Detta kommer att producera outputen "HELLO WORLD".

## Djupdykning:

Konventionen att kapitalisera strängar härstammar från det engelska språket där det används för att skilja mellan ord som börjar med stor eller liten bokstav. Det finns också andra sätt att få en sträng kapitaliserad, som att använda ```Substring()``` och ```String.Format()```.

Det är viktigt att komma ihåg att metoder som ```ToUpper()``` inte ändrar själva strängen, utan returnerar en ny kapitaliserad sträng. Om du vill ändra den ursprungliga strängen måste du tilldela det nya värdet till den ursprungliga variabeln.

## Se även:

- [Officiell dokumentation för C# String.ToUpper()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1)
- [Alternativa sätt att kapitalisera strängar i C#](https://stackoverflow.com/questions/9645731/substring-first-letter-of-each-word-of-a-string-in-c-sharp)