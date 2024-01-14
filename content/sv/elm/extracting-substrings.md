---
title:                "Elm: Utvinning av delsträngar"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför
Att utvinna substrängar är en vanlig uppgift inom programmering, och det kan vara användbart i många olika situationer. En vanlig anledning är att man vill manipulera en endast del av en sträng, istället för hela strängen. Det kan också användas för att söka efter specifika ord eller uttryck inom en sträng.

## Hur man gör
Ett enkelt sätt att extrahera substrängar i Elm är att använda funktionen 'String.slice'. Denna funktion tar emot en startposition och en slutposition, och returnerar den del av strängen som är mellan dessa positioner. Till exempel:

```Elm
String.slice 2 5 "Hej alla Elm utvecklare" 
```

Detta skulle returnera substrängen "j al". 

För att extrahera en del av en sträng baserat på ett specifikt ord eller uttryck kan man istället använda funktionen 'String.dropLeft' eller 'String.dropRight'. Dessa funktioner tar emot en sträng och en annan sträng som sökord, och returnerar den del av strängen som kommer efter eller före sökordet. Till exempel:

```Elm
String.dropLeft "Hej alla Elm utvecklare" "all"
```

Detta skulle returnera substrängen "a Elm utvecklare".

## Djupdykning
När man använder funktionen 'String.slice', är det viktigt att komma ihåg att start- och slutpositioner är indexbaserade. Detta betyder att den första positionen i en sträng är index 0, och den sista positionen är index (längden på strängen - 1). Om man anger en start- eller slutposition som är utanför dessa gränser, kommer funktionen att returnera en tom sträng.

Man kan också använda funktionerna 'String.take' och 'String.drop' för att extrahera en del av en sträng baserat på en bestämd längd istället för en position. Dessa funktioner tar emot en startposition och sedan ett antal tecken som ska tas eller tas bort. Till exempel:

```Elm
String.take 3 "Hej alla" 
```

Detta skulle returnera substrängen "Hej".

## Se även
- [Officiell dokumentation för stränghantering i Elm](https://package.elm-lang.org/packages/elm/core/latest)
- [En guide för att arbeta med strängar i Elm](https://medium.com/@TylersGit/introduction-to-strings-in-elm-120f27df8f9c)
- [En samling av olika kodexempel för att arbeta med strängar i Elm](https://www.programming-idioms.org/idiom/67/substring/439/elm)