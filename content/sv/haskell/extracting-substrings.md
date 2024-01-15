---
title:                "Extrahering av delsträngar"
html_title:           "Haskell: Extrahering av delsträngar"
simple_title:         "Extrahering av delsträngar"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför någon skulle vilja extrahera substrängar? Ibland kan det vara nödvändigt att dela upp en större sträng i mindre delar för att bearbeta eller söka efter specifika delar av informationen.

## Hur man gör det

Att extrahera substrängar är enkelt med hjälp av inbyggda funktioner i Haskell. För att prova det själv, öppna din Haskell-miljö och följ dessa steg:

1. Skapa en sträng som du vill extrahera delar från. Till exempel: ```Haskell "Hej alla Haskell-älskare!"```

2. Använd funktionen "take" för att extrahera en del av strängen, baserat på ett angivet antal tecken. Till exempel: ```Haskell take 3 "Hej alla Haskell-älskare!"``` kommer att returnera "Hej".

3. Använd funktionen "drop" för att ta bort en del av strängen, baserat på ett angivet antal tecken. Till exempel: ```Haskell l "Hej alla Haskell-älskare!"``` kommer att returnera "lla Haskell-älskare!".

4. Använd funktionerna "takeWhile" och "dropWhile" för att extrahera delar av strängen baserat på ett villkor. Till exempel: ```Haskell takeWhile (\x -> x == 'a') "aaaaabbbccc"``` kommer att returnera "aaaaa", medan ```Haskell dropWhile (\x -> x == 'a') "aaaaabbbccc"``` kommer att returnera "bbbccc".

## Djupdykning

Förutom de nämnda funktionerna finns det många fler sätt att extrahera substrängar i Haskell. Här är några tips för dig som vill fördjupa dig i ämnet:

- Använd funktionerna "splitAt" eller "span" för att dela upp strängen vid ett visst index eller villkor.
- Ta reda på mer om listkomprehension och hur den kan användas för att skapa listor av substrängar baserat på ett villkor eller mönster.
- Upptäck andra string manipulation bibliotek som kan vara användbara för dina specifika behov.

## Se även

- [Haskell String Manipulation](https://wiki.haskell.org/Strings)
- [Hackage - String Libraries](https://hackage.haskell.org/packages/search?terms=strings)
- [Programming in Haskell by Graham Hutton](http://www.cs.nott.ac.uk/~pszgmh/pih.html)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)