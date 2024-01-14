---
title:                "Fish Shell: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
I enkel kod, ibland måste du kombinera flera strängar tillsammans för att skapa en längre streng. Fish Shell går utöver bara grundläggande strängkonkaterning, och ger dig kraftfulla verktyg för att manipulera och hantera strängar på ett mer avancerat sätt.

## Hur man gör
För att konkatenera strängar i Fish Shell, använder du kommandot `string join`, följt av de strängar du vill kombinera. Till exempel:

```Fish Shell
string join "Hej" "pa" "dig!"
```

Outputen skulle vara:

```
Hej på dig!
```

Du kan också använda variabler i `string join` kommandot för att kombinera med andra strängar. Till exempel:

```Fish Shell
set namn "John"
string join "Hej" $namn ", hur mår du?"
```

Outputen skulle vara:

```
Hej John, hur mår du?
```

Du kan till och med använda `string join` för att skapa strängar baserat på regulatoriska uttryck (regular expressions). Till exempel, om du ville ta bort alla vokaler från en sträng, kan du göra det med hjälp av `string join` på följande sätt:

```Fish Shell
set sträng "Hej pa dig!"
set formateradstrang (string join "" (string match -r "[aeiou]" -r " " $sträng))
echo $formateradstrang
```

Outputen skulle vara:

```
Hj p dg!
```

## Djupdykning
När du konkatenerar strängar i Fish Shell, används variabler som referenser till de ursprungliga strängarna. Detta innebär att om du ändrar en variabel senare, påverkas också slutsträngen. Till exempel:

```Fish Shell
set namn "Anna-Lena"
set halsning "Hej " $namn
set namn "Johan"
echo $halsning
```

Outputen skulle vara:

```
Hej Johan
```

Detta händer eftersom `halsning` variabeln fortfarande refererar till `Anna-Lena` och inte har uppdaterats med den nya värdet på `namn`. Detta är en viktig sak att komma ihåg när du arbetar med strängkonkaterning i Fish Shell.

## Se även
* [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
* [Guide till reguljära uttryck i Fish Shell](https://fishshell.com/docs/current/tutorial.html#tut_shallow_learning)
* [Fish Shell community forum](https://github.com/fish-shell/fish-shell/discussions)