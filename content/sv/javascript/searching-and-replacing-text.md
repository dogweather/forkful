---
title:                "Javascript: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en vanlig uppgift inom programmering och kan vara användbart för att effektivisera arbetsflödet. Genom att använda rätt metoder och verktyg kan du enkelt byta ut ord eller fraser i en längre text.

## Hur man gör Det
För att söka och ersätta text i Javascript behöver du använda dig av två metoder: `search()` och `replace()`. Båda metoderna används på en sträng och tar emot argument för sökordet och ersättningen.

För att använda `search()` metoden, börja med att definiera en sträng som du vill söka igenom. Sedan kan du ange vilket ord eller fras du vill söka efter inom parenteserna efter metoden. T.ex. `stringToSearch.search("sökord")`. Metoden returnerar indexet för det första förekomsten av sökordet i strängen eller `-1` om sökordet inte finns.

För att använda `replace()` metoden, gör på samma sätt med att definiera en sträng och ange då sökordet och ersättningen inom parenteserna efter metoden. T.ex. `stringToReplace.replace("sökord", "ersättning")`. Metoden returnerar en ny sträng med sökordet ersatt av den angivna ersättningen.

```Javascript
let myString = "Jag älskar att programmera, det är som en gåta som jag älskar att lösa.";
console.log(myString.search("älskar")); // output: 4
console.log(myString.replace("älskar", "avskyr")); // output: Jag avskyr att programmera, det är som en gåta som jag älskar att lösa.
```

## Deep Dive
När du använder `replace()` metoden kan du också använda en regular expression som sökord. Detta är användbart om du vill byta ut flera olika varianter av ett ord eller om du vill vara mer flexibel med vad sökordet kan vara. För att göra detta måste du använda dig av en `g` modifier efter regular expressionen, vilket står för "global" och kommer att leta efter alla förekomster istället för bara den första.

Du kan också använda `replace()` metoden med en callback funktion, som kommer att köras för varje matchning som hittas i strängen. I callback funktionen kan du definiera den specifika träff du vill byta ut samt hur den ska bytas ut.

```Javascript
let myString = "Det finns många olika sätt att skriva ett blogginlägg. Och vissa är bättre än andra.";
console.log(myString.replace(/blogginlägg/g, function(match){return match.toUpperCase()})); // output: Det finns många olika sätt att skriva ett BLOGGINLÄGG. Och vissa är bättre än andra.
```

## Se även
- [Javascript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Javascript Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)