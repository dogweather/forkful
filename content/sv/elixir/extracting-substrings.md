---
title:    "Elixir: Extrahering av delsträngar"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt måste vi hantera textsträngar på olika sätt. Ibland kan vi behöva extrahera delar av en sträng för att få ut specifik information eller anpassa den på ett särskilt sätt. I Elixir finns det ett antal inbyggda funktioner som hjälper oss att hantera detta.

## Så här gör du

För att extrahera substrängar i Elixir kan du använda funktionen `String.slice/3`. Den tar tre argument: den ursprungliga strängen, startpositionen och slutpositionen för den del av strängen som du vill extrahera. Låt oss se på ett exempel:

```Elixir
str = "Välkommen till Elixir-världen!"
String.slice(str, 11, 17)
```

Detta kommer att returnera "Elixir", som är substrängen som börjar på position 11 och slutar på position 17.

Om du vill extrahera en del av strängen från en visst position till slutet av strängen kan du använda funktionen `String.slice/2`, som endast tar två argument - den ursprungliga strängen och startpositionen. Låt oss se ett exempel på detta:

```Elixir
str = "Jag älskar programmering!"
String.slice(str, 11)
```

Detta kommer att returnera "programmering!", eftersom vi inte specificerar någon slutposition och därmed returnerar strängen från position 11 till slutet av strängen.

## Djupdykning

När det gäller strängar i Elixir finns det en mängd inbyggda funktioner som du kan använda för att bearbeta och manipulera dem. När du extraherar en substräng, se till att utgå från startposition 0, inte 1. Detta beror på att Elixir behandlar strängar som listor av tecken. Till exempel, om vi har en sträng som "Hello world!" kommer varje tecken i strängen att representeras som en element i listan.

En annan viktig sak att komma ihåg är att positioner i Elixir alltid är noll-indexerade, vilket innebär att den första positionen är 0, inte 1. Detta är viktigt att tänka på eftersom det kan påverka hur du specificerar start- och slutpositioner när du extraherar substrängar.

## Se även

- Officiell dokumentation för `String.slice/3`: https://hexdocs.pm/elixir/String.html#slice/3
- En handledning om strängmanipulering i Elixir: https://elixir-lang.org/getting-started/string-interpolation-and-manipulation.html
- Mina andra blogginlägg om Elixir: [länk], [länk], [länk]