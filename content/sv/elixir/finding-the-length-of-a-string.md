---
title:                "Elixir: Att hitta längden på en sträng"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande uppgift som kan vara användbar i en mängd olika scenarier när du programmerar med Elixir. Det kan hjälpa dig att manipulera strängar, kontrollera indata och utföra andra operationer som kräver kunskap om strängens längd. Det är också en bra övning för att bygga upp din förståelse för grundläggande Elixir-funktioner och syntax.

## Så här gör du

För att hitta längden på en sträng i Elixir, används funktionen `String.length/1`. Denna funktion tar en sträng som inmatning och returnerar dess längd som ett heltal. Se nedan för ett exempel på hur detta kan se ut i Elixir:

```Elixir
string = "Hej världen!"
String.length(string)
```

Detta skulle ge följande utmatning:

```Elixir
12
```

Som du kan se beräknar funktionen `String.length/1` längden på strängen genom att räkna antalet tecken, inklusive mellanslag och skiljetecken. Detta betyder att den inte bara tar hänsyn till bokstäverna, utan också till alla andra tecken som är en del av strängen.

Nu när du vet hur man använder funktionen `String.length/1`, kan du experimentera med den på olika sätt och kombinera den med andra Elixir-funktioner för att uppnå olika resultat.

## Djupdykning

En intressant sak att notera är att Elixir har flera inbyggda funktioner för att hantera strängar, inklusive `String.length/1` som vi har sett ovan. Men det finns också andra sätt att hitta längden på en sträng i Elixir. Till exempel kan du använda mönstermatchning och rekursion för att skapa en egen funktion som räknar tecken i en sträng.

Det finns också vissa viktiga skillnader att vara medveten om när man hanterar strängar i Elixir jämfört med andra programmeringsspråk. Till exempel är strängar i Elixir inte muterbara, vilket innebär att de inte kan ändras direkt. Istället måste du använda funktioner som `String.replace/3` för att göra ändringar i en sträng.

## Se också

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Elixir String Functions Cheat Sheet](https://devhints.io/elixir-strings)
- [Elixir Strings Tutorial](https://elixirschool.com/sv/lessons/basics/strings/)