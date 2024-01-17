---
title:                "Interpolering av en sträng"
html_title:           "Elm: Interpolering av en sträng"
simple_title:         "Interpolering av en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Vad och Varför?
Interpolation av en sträng är en metod som används inom programmering för att sammanfoga en textsträng med variabler eller värden. Det gör det möjligt för oss att dynamiskt skapa textsträngar som kan anpassas efter olika värden eller variabler. Detta gör det enklare för utvecklare att skapa mer dynamiska och anpassningsbara applikationer.

# Hur gör man?
Elm har ett inbyggt sätt att interpolera strängar genom att använda { } tecken runt variabler eller värden som ska ersättas. Enkelt uttryckt ser syntaxen ut så här:

Elm

```
"Jag gillar {favoritMat}" 
```

När vi sedan tilldelar värdet favoritMat till en variabel, kommer den att ersättas i strängen. Exempelvis:

Elm

```
favoritMat = "pizza" 
"Jag gillar pizza"
```

Vi kan också använda flera värden i samma sträng genom att separera dem med kommatecken. Exempelvis:

Elm

```
favoritMat = "pizza" 
favoritDryck = "läsk" 
"Jag gillar {favoritMat} och {favoritDryck}"
```
Denna sträng kommer att ersättas med "Jag gillar pizza och läsk".

# Djupdykning
Interpolation av strängar är inte något unikt för Elm. Det finns liknande funktioner i många andra programmeringsspråk som Java och JavaScript. Skillnaden är att i Elm hanteras detta genom en inbyggd funktion, medan det i andra språk ofta kräver att man använder en extern biblioteksfunktion.

En annan metod för att hantera dynamiska strängar är konkatenering, vilket innebär att man sammanfogar flera strängar till en enda sträng. Men detta kan bli mer komplicerat och svårare att läsa än att använda interpolation.

I implementationen av interpolation i Elm använder man sig av funktionen `String.format`, där man först anger strängen och sedan variablerna som ska ersättas. Detta gör det möjligt att hantera olika datatyper i samma sträng, till exempel text och tal.

# Se även
För mer information om stränginterpolation i Elm, se [Elm Dokumentation](https://guide.elm-lang.org/) och [Elm By Example Interactive Course](https://elmprogramming.com/).