---
title:                "Fish Shell: Hitta längden på en sträng"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hitta längden på en textsträng är en grundläggande funktion som kan användas för att lösa olika programmeringsproblem. Det är också en viktig del av att förstå hur en dator bearbetar och behandlar textdata.

## Så här gör du

För att hitta längden på en sträng i Fish Shell, använder du kommandot "count" följt av strängen du vill mäta. Här är ett exempel på hur du kan använda det:

```Fish Shell
count "Hej världen"

```

Output: 11

Detta berättar för oss att det finns 11 tecken i strängen "Hej världen", inklusive mellanslag.

Om du vill mäta längden på en variabel istället för en direkt sträng, använder du variabelnamnet i stället för den faktiska strängen. Låt oss säga att vi har en variabel som heter "namn" med värdet "Sara". Vi kan då använda "count" för att hitta längden på variabeln "namn":

```Fish Shell
set namn "Sara"
count $namn

```

Output: 4

Observera att vi använder $"variabelnamn" för att hämta värdet av en variabel i Fish Shell.

## Djupdykning

Att hitta längden på en sträng verkar kanske enkelt, men det finns faktiskt en del intressanta saker att lära sig om detta enkla kommando.

För det första, så räknas inte bara bokstäverna i en sträng utan också alla andra tecken. Till exempel skulle strängen "Hej! Välkommen!" ha en längd på 14, eftersom utropstecknet räknas som ett tecken.

Dessutom har "count" en inbyggd funktion för att mäta längden på en lista, då det fungerar på samma sätt som med en sträng. Till exempel, om vi har en lista med olika djurarter och vi vill veta hur många st finns det, kan vi använda "count" för att få svaret.

```Fish Shell
set djurist (fisk ko get)
count $djurlist

```

Output: 3

Sedan finns det en mer avancerad version av "count" som heter "count -r", som returnerar längden på strängen utan att räkna med mellanslag och andra tomma tecken. Om vi använder detta på vår tidigare exempelsträng "Hej världen", blir svaret 10 istället för 11 eftersom mellanslaget inte räknas.

## Se även

- Fish Shell's documentation on [count](https://fishshell.com/docs/current/cmds/count.html)
- Användbara Fish Shell-kommandon för nybörjare [link here]
- Hur man skapar en enkel "Hello World" applikation med Fish Shell [link here]