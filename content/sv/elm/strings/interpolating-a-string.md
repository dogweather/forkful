---
date: 2024-01-20 17:50:49.614551-07:00
description: "S\xE5 h\xE4r g\xF6r du: Elm har ingen inbyggd str\xE4nginterpolering\
  \ som man kanske \xE4r van vid fr\xE5n andra spr\xE5k. Ist\xE4llet anv\xE4nder man\
  \ funktioner som `String.concat`\u2026"
lastmod: '2024-03-13T22:44:37.815450-06:00'
model: gpt-4-1106-preview
summary: "Elm har ingen inbyggd str\xE4nginterpolering som man kanske \xE4r van vid\
  \ fr\xE5n andra spr\xE5k."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Så här gör du:
Elm har ingen inbyggd stränginterpolering som man kanske är van vid från andra språk. Istället använder man funktioner som `String.concat` eller `++` operatören för att slå ihop strängar.

```Elm
name = "Världen"
greeting = "Hej " ++ name ++ "!"

-- Output: "Hej Världen!"
```

Eller för mer komplexa situationer kan man skapa en funktion:

```Elm
helloTo : String -> String
helloTo name = 
    "Hej " ++ name ++ "!"

main = 
    helloTo "Elm programmerare"

-- Output: "Hej Elm programmerare!"
```

## Fördjupning
Stränginterpolering som koncept har varit runt länge, men olika språk hanterar det olika. Till exempel, JavaScript använder template literals (\`${variable}\`) och Python använder f-strängar. Elm väljer enklare sammanfogning för att behålla språket rent och funktionellt. Även om det kan kännas omständligt först, leder det till tydligare kod där alla strängmanipulationer är uttryckliga.

Till alternativen hör att bygga egna interpoleringsfunktioner eller att använda externa paket som `elm-string-interpolate` för att närma sig interpolering så som den finns i andra språk.

Vad gäller implementering är det viktigt att komma ihåg att Elm är kompilerat, vilket betyder att alla strängoperationer måste omvandlas till effektiv JavaScript-kod under huven.

## Se även
- Elm documentation om strängar: https://package.elm-lang.org/packages/elm/core/latest/String
- 'elm-string-interpolate' paketet: https://package.elm-lang.org/packages/lattenwald/elm-string-interpolate/latest/
- Elm-lärdomar om sammanfogning av strängar: https://elmprogramming.com/string-concatenation.html
