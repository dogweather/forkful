---
title:    "Fish Shell: Kapitalisera en sträng"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera en sträng till versaler är en vanlig uppgift inom programmering, och det kan vara användbart i många olika kontexter. Oavsett om du vill formatera utskrifter, filtrera data eller manipulera text, kan det vara användbart att kunna konvertera en sträng till versaler.

## Hur man gör det

Det finns några olika sätt att konvertera en sträng till versaler i Fish Shell. Ett sätt är att använda inbyggda kommandon som `sed` eller `tr` för att manipulera strängen. Till exempel kan du använda `tr` för att konvertera alla tecken i en sträng till versaler genom att skriva:

```
Fish Shell code block:

echo "hej, mina vänner!" | tr '[:lower:]' '[:upper:]'
```

Output:

```
HEJ, MINA VÄNNER!
```

En annan metod är att använda ett inbyggt kommando som heter `capitalize`, som automatiskt konverterar den första bokstaven i varje ord till en versal bokstav. Till exempel:

```
Fish Shell code block:

echo "välkommen till fish shell" | capitalize
```

Output:

```
Välkommen Till Fish Shell
```

## Djupdykning

I Fish Shell finns det flera inbyggda kommandon som kan hjälpa dig att konvertera strängar till versaler. Du kan även använda andra programmeringsspråk som till exempel Python eller Perl för att utföra samma uppgift. Det är viktigt att förstå hur dessa kommandon fungerar för att kunna utföra mer avancerade operationer med strängar.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/)
- [Fish Shell Cookbook](https://fishshell.com/docs/current/index.html#cookbook)