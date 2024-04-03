---
date: 2024-01-20 18:03:36.765293-07:00
description: "Att starta ett nytt projekt inneb\xE4r att s\xE4tta upp en ny kodbas\
  \ fr\xE5n grunden. Programmerare g\xF6r det f\xF6r att skapa unika l\xF6sningar\
  \ eller utforska nya id\xE9er."
lastmod: '2024-03-13T22:44:37.955829-06:00'
model: gpt-4-1106-preview
summary: "Att starta ett nytt projekt inneb\xE4r att s\xE4tta upp en ny kodbas fr\xE5\
  n grunden."
title: "Att p\xE5b\xF6rja ett nytt projekt"
weight: 1
---

## Så här gör du:
För att skapa nytt Haskell-projekt rekommenderar vi att använda Stack, ett kraftfullt verktyg som hanterar projektets bygge och beroenden.

```Haskell
-- Installera Stack
$ curl -sSL https://get.haskellstack.org/ | sh

-- Skapa ett nytt projekt
$ stack new myproject

-- Bygga projektet, detta skapar en exekverbar fil
$ cd myproject
$ stack build

-- Kör projektet
$ stack exec myproject-exe
```

Output när projektet skapas:

```
Downloading template "new-template" to create project "myproject" in myproject/ ...
The project 'myproject' has been created and is located at /path/to/myproject
```

## Djupdykning
Stack är utvecklat med syftet att göra det smidigt att arbeta med Haskell-projekt. Det kom som ett alternativ till det äldre systemet Cabal, som fortfarande används men anses av många som mer komplicerat. Stack använder sig av Stackage, en stor samling av Haskell-paket som är testade tillsammans för att säkerställa kompatibilitet. Detta betyder mindre beroendeproblem för utvecklaren. 

Att starta ett projekt i Haskell var förr en mer manuell process. Alternativ inkluderar att direkt använda GHC (Glasgow Haskell Compiler), men detta kräver mer konfiguration. Med Stack kan du skapa projekt som bygger på olika förkonfigurerade mallar, vilket hjälper dig att komma igång snabbt och med god ordning från början.

## Se även
- [Haskell Stack Documentation](https://docs.haskellstack.org/en/stable/README/)
- [Stackage Server](https://www.stackage.org/)
