---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:41.696549-07:00
description: "Hur man g\xF6r: I VBA definieras funktioner med hj\xE4lp av uttrycken\
  \ `Function` och `End Function`. H\xE4r \xE4r ett enkelt exempel p\xE5 hur man skapar\
  \ en funktion som\u2026"
lastmod: '2024-03-13T22:44:37.749139-06:00'
model: gpt-4-0125-preview
summary: "I VBA definieras funktioner med hj\xE4lp av uttrycken `Function` och `End\
  \ Function`."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
I VBA definieras funktioner med hjälp av uttrycken `Function` och `End Function`. Här är ett enkelt exempel på hur man skapar en funktion som beräknar arean av en rektangel:

```basic
Function CalculateArea(längd As Double, bredd As Double) As Double
    CalculateArea = längd * bredd
End Function
```

För att anropa denna funktion i din VBA-kod och visa resultatet i en meddelanderuta skulle du använda:

```basic
Sub ShowArea()
    Dim area As Double
    area = CalculateArea(10, 5)
    MsgBox "Arean är " & area
End Sub
```

När den här koden körs, visar den en meddelanderuta som säger: `Arean är 50`.

### Skicka variabler ByRef och ByVal
VBA tillåter dig att skicka variabler till funktioner antingen genom referens (`ByRef`) eller genom värde (`ByVal`). Det förra innebär att den ursprungliga variabeln kan modifieras av funktionen, medan det senare skickar en kopia, vilket skyddar den ursprungliga variabeln från förändringar.

```basic
Function ModifyValue(ByRef num As Integer)
    num = num + 5
End Function

Function PreserveValue(ByVal num As Integer) As Integer
    num = num + 5
    PreserveValue = num
End Function
```

## På djupet
VBA, som ett händelsedrivet programmeringsspråk, lägger stor vikt vid funktioner och subrutiner för att hantera olika uppgifter. Till skillnad från många moderna språk har VBA en unik egenskap där nyckelordet `Function` inte bara deklarerar en block av återanvändbar kod utan också tillåter ett implicit returvärde som direkt tilldelas funktionens namn.

Historiskt sett har designen av VBA-funktioner påverkats av tidigare programmeringsparadigmer där inkapsling och modularitet gradvis erkändes för deras betydelse i mjukvaruutveckling. Denna historiska bakgrund har lett till att VBA antagit en något konservativ men funktionell ansats till att organisera kod.

Även om VBA är kraftfullt inom sina stammiljöer (t.ex. Microsoft Office-applikationer), är det väsentligt att notera att programmeringsvärlden har utvecklats. Språk som Python erbjuder en enklare syntax och ett omfattande standardbibliotek, vilket gör dem till ett fördelaktigt alternativ för olika applikationer utanför Office-paketet. Men när man arbetar inom Microsoft Office-produkter är integrations- och automatiseringsmöjligheterna som VBA erbjuder oöverträffade.

Det är värt att notera att trots sin ålder är gemenskapen kring VBA fortfarande aktiv, och fortsätter att hitta innovativa sätt att utnyttja dess funktionalitet. Men eftersom programvaruindustrin rör sig mot mer moderna, mångsidiga och robusta språk, uppmuntras programmerare som är bekanta med VBA att utforska dessa alternativ för uppgifter som inte är relaterade till Office för att bredda sitt kodningsverktyg.
