---
title:    "Elixir: Kapitalisera en sträng"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle du vilja konvertera en sträng till versaler? Det finns flera användbara tillämpningar för detta, till exempel om du vill jämföra texter som ska vara oberoende av versaler och gemener, eller om du vill göra en text mer läsbar genom att konvertera den till en större textstorlek.

## Så här gör du
För att konvertera en sträng till versaler i Elixir, kan du använda funktionen `String.upcase/1`. Detta är en inbyggd funktion som tar en sträng som argument och returnerar en ny sträng med alla bokstäver i versaler. Här är ett exempel på hur du kan använda den:

```Elixir
iex> String.upcase("hej") 
"HEJ"
```

Som du kan se, har vår sträng "hej" blivit konverterad till "HEJ". Det är viktigt att notera att `String.upcase/1` inte modifierar den ursprungliga strängen, utan returnerar bara en ny sträng med de ändrade bokstäverna.

Om du vill konvertera en sträng till gemener kan du istället använda funktionen `String.downcase/1`. Du kan också använda funktionen `String.capitalize/1` för att konvertera den första bokstaven i en sträng till en versal och resten till gemener.

```Elixir
iex> String.downcase("HEJ") 
"hej"

iex> String.capitalize("hej, det här är min sträng") 
"Hej, det här är min sträng"
```

## Djupdykning
Det finns flera saker att tänka på när du ska konvertera en sträng till versaler. En är att det finns skillnader mellan hur olika språk hanterar versaler och gemener. Till exempel, i de flesta västerländska språk är det enkelt att konvertera till versaler eftersom de flesta bokstäver har en motsvarande versalform. Men i vissa språk, som till exempel turkiska, så finns det bokstäver som saknar motsvarande versaler, vilket kan leda till felaktig konvertering.

Det är också viktigt att tänka på att inte alltid anta att versalformen av en bokstav är bara en större version av gemener. Till exempel i tyska så har bokstaven "ß" (ess-tsett) ingen versalform, utan ska istället bytas ut mot "SS" i versaler.

## Se även
- [String.upcase/1 i Elixir Docs](https://hexdocs.pm/elixir/String.html#upcase/1)
- [Turkiska och versaler](https://en.wikipedia.org/wiki/Turkish_alphabet#Case)
- [Versaler och tyska språket](https://en.wikipedia.org/wiki/Capital_%C3%9F)