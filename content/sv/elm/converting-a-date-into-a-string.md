---
title:    "Elm: Omvandla ett datum till en sträng"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför
Att konvertera ett datum till en sträng är en vanlig uppgift när man arbetar med datum och tidsformat. Genom att använda Elm kan man enkelt konvertera ett datum till en sträng för att använda det i sin applikation.

## Så här gör du
Först måste vi importera elm/time biblioteket för att kunna använda funktioner för att hantera datum. Sedan kan vi använda funktionen `format` för att konvertera ett datum till en sträng enligt önskat format. Se nedan för ett enkelt exempel:

```Elm
import Time
Time.format "%Y-%m-%d" (Time.now Time.utc)
```

Detta kommer att ge oss ett resultat som ser ut som följande: `2022-02-16`. Vi kan också lägga till fler formatfunktioner för att få ut mer information, till exempel dag i veckan eller detektera dagens datum.

```Elm
import Time
formatTodaysDate format =
  Time.now Time.utc
  |>Time.format format

formatTodaysDate "%d %B %Y"
```

Detta kommer att ge oss en sträng som ser ut enligt följande: `16 februari 2022`.

## Djupdykning
När vi tittar närmare på konvertering av datum till sträng i Elm, kan vi använda oss av följande formatfunktioner:

| Funktion          | Beskrivning                                                       |
| ----------------- | ----------------------------------------------------------------- |
| %Y                | Fyr-siffrigt år                                                   |
| %m                | Två-siffrigt månad (01-12)                                        |
| %B                | Hela namnet på månaden                                            |
| %d                | Två-siffrigt dag (01-31)                                          |
| %A                | Hela namnet på dagen                                              |
| %p                | AM eller PM                                                       |
| %H                | Timmar i 24-timmarsformat                                         |
| %I                | Timmar i 12-timmarsformat                                         |
| %M                | Minuter                                                           |
| %S                | Sekunder                                                          |
| %w                | Veckodag som en siffra (0-6, där 0 är söndag)                     |
| %Z                | Tidszon (t.ex. UTC eller GMT)                                     |

Genom att använda dessa formatfunktioner kan du anpassa din sträng till det format som passar bäst för din applikation.

## Se även
- Officiell dokumentation för time biblioteket: https://package.elm-lang.org/packages/elm/time/latest/
- Blogginlägg om att hantera datum och tid i Elm: https://www.elmprogramming.com/handling-date-and-time-in-elm.html
- Elmcasts avsnitt om hantering av tid i Elm: https://elm-casts.com/episodes/handling-time