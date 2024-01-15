---
title:                "Börja ett nytt projekt"
html_title:           "Haskell: Börja ett nytt projekt"
simple_title:         "Börja ett nytt projekt"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Varför

Du kanske har hört talas om Haskell och är nyfiken på att lära dig det, eller så har du redan börjat använda det och vill starta ett nytt projekt. Oavsett anledning är Haskell ett kraftfullt, funktionellt programmeringsspråk som kan hjälpa dig att skriva effektiv och elegant kod.

## Så här gör du

För att börja ett nytt projekt i Haskell behöver du först installera en Haskell-miljö på din dator. Det finns olika verktyg som kan hjälpa dig med detta, men det enklaste är att ladda ner Haskell Platform, som innehåller allt du behöver för att komma igång.

När du har installerat Haskell Platform kan du skapa en ny mapp för ditt projekt och öppna den i en textredigerare eller utvecklingsmiljö. Sedan är det dags att börja skriva kod!

En av de viktigaste delarna i Haskell är typer. Detta hjälper dig att undvika felaktig kod och gör det enklare att förstå din programkod. Här är ett exempel på en funktion som tar emot två heltal och returnerar deras summa:

```Haskell
addTwo :: Int -> Int -> Int
addTwo x y = x + y
```

För att köra denna funktion i din terminal eller kompilator, skriv:

```Haskell
addTwo 5 7
```

Outputen bör vara 12.

Du kan också använda Haskell för att skriva mer komplexa program som t.ex. webbapplikationer. Ett populärt ramverk för detta är Yesod, som hjälper dig att bygga webbapplikationer med Haskell.

## Djupdykning

En av de bästa sakerna med att använda Haskell för nya projekt är den inbyggda typinferensen. Detta betyder att du inte alltid behöver ange typer i din kod, eftersom Haskell kan dra slutsatser baserat på hur du använder dina funktioner.

Haskell är också ett starkt typat språk, vilket betyder att det har ett starkt system för att hantera datatyper och deras relationer. Detta gör det lättare att skriva robust och säker kod.

En annan fördel med Haskell är dess hög ordningens funktioner. Detta innebär att du kan behandla funktioner som vanliga värden och skicka dem som argument till andra funktioner. Detta ger stor flexibilitet och möjlighet att abstrahera på en högre nivå.

## Se även

- [Haskell.org](https://www.haskell.org/)
- [Haskell Platform](https://www.haskell.org/platform/)
- [Haskell wiki](https://wiki.haskell.org/)
- [Yesod](https://www.yesodweb.com/)