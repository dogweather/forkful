---
title:                "Elixir: Att göra en sträng stor bokstav"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Om du är ny inom programmering, har du kanske hört talas om "capitalizing a string" men inte riktigt förstått vad det betyder eller varför det är viktigt. Om du använder Elixir, kan det verka som ett Ytligt koncept, men det är faktiskt en viktig del av att hantera textsträngar i ditt program. Låt oss utforska varför det är relevant och hur du kan göra det i Elixir.

## Hur man gör

Först måste vi förstå vad det innebär att "capitalize a string". Programmerspråk följer ofta strikta regler för hur data ska formateras, och detta gäller även för textsträngar. När en sträng är "capitalized", betyder det att den första bokstaven i varje ord är en stor bokstav, medan resten av bokstäverna är små. Detta är viktigt för att göra texten mer läsbar och enhetlig.

För att göra detta i Elixir, kan du använda funktionen `String.capitalize/1`, som tar in en sträng som argument och returnerar den med den första bokstaven i varje ord som en stor bokstav. Se ett exempel nedan:

```Elixir
String.capitalize("hej alla") #=> "Hej Alla"
```

Som du kan se, är "h" i "hej" och "a" i "alla" nu kapitaliserade. Detta gör det lättare att läsa och ser mer professionellt ut. Om du vill capitalizera hela strängen, oavsett separation mellan ord, kan du använda `String.upcase/1` och `String.downcase/1` för att göra alla bokstäver stora respektive små.

## Djupdykning

En intressant sak att notera är att `String.capitalize/1` också tar hänsyn till specifika tecken i andra språk. Till exempel, om du har en sträng på svenska som börjar med "ö", kommer det att capitalizera som "Ö". Detta bevisar att Elixir är ett mycket mångsidigt och internationellt programmeringsspråk.

Det är också viktigt att nämna att funktionen `String.capitalize/1` returnerar en kopia av den ursprungliga strängen, och ändrar inte den ursprungliga strukturen. Detta är en viktig aspekt att förstå när du arbetar med textsträngar i Elixir.

## Se även

- Officiell Elixir-dokumentation för `String.capitalize/1`: https://hexdocs.pm/elixir/String.html#capitalize/1
- Tutorial om att arbeta med textsträngar i Elixir: https://elixircasts.io/working-with-strings-in-elixir
- Översikt över Elixir-programmeringsspråket: https://elixir-lang.org/