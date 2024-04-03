---
date: 2024-01-26 00:51:11.247497-07:00
description: "Felhantering handlar om att hantera det ov\xE4ntade i program - som\
  \ en ordningsvakt som hanterar br\xE5kstakar. Programmerare f\xF6redrar n\xE4r allt\
  \ flyter p\xE5;\u2026"
lastmod: '2024-03-13T22:44:37.532631-06:00'
model: gpt-4-1106-preview
summary: "Felhantering handlar om att hantera det ov\xE4ntade i program - som en ordningsvakt\
  \ som hanterar br\xE5kstakar."
title: Hantering av fel
weight: 16
---

## Hur man gör:
Clojure, i likhet med sina Lisp-förfäder, förlitar sig på undantag (exceptions) för att hantera fel. Så här visar du vad du går för när saker och ting går åt skogen.

Att kasta ett undantag är enkelt:
```Clojure
(throw (Exception. "Oops! Något gick fel."))
```

Att fånga ett undantag, något du kommer att göra ofta:
```Clojure
(try
  ;; riskabel kod
  (/ 1 0)
  (catch ArithmeticException e
    (println "Kan inte dela med noll!"))
  ;; finally-blocket körs oavsett
  (finally 
    (println "Städkod går här.")))
```
Exempel på utskrift för ovanstående catch-block:
```
Kan inte dela med noll!
Städkod går här.
```

Användning av `ex-info` och `ex-data` för rikare kontext om undantag:
```Clojure
(try
  ;; orsaka ett anpassat undantag
  (throw (ex-info "Anpassat fel" {:type :custom-failure}))
  (catch Exception e
    ;; att få ut datan från vårt anpassade undantag
    (println (ex-data e))))
```
Exempel på utskrift:
```
{:type :custom-failure}
```

## Fördjupning
Felhanteringsberättelsen i Clojure är inte radikalt annorlunda från andra Lisps eller ens Java (från vilket den ärver `try-catch`-mekanismen). Det är pragmatiskt; att använda undantag är huvudvägen, precis som i Java, men Clojure erbjuder en funktionell touch med `ex-info` och `ex-data` för rikare feldata.

Alternativ för felhantering i Clojure inkluderar att använda monadiska konstruktioner, såsom `either`-monaden från bibliotek som `cats`, eller core.async för kanalbaserad felpropagering. Dessa är dock mer komplexa och används i specifika scenarion.

Historiskt sett har felhantering i programmeringsspråk utvecklats från enkla statusreturer till de mer sofistikerade undantagshanteringssystemen i moderna språk. Clojure väljer enkelhet och en nypa funktionell programmering, som blandar gammalt och nytt.

## Se även
- Clojures guide till undantag: https://clojure.org/guides/exceptions
- “Cats” bibliotek för mer funktionella tillvägagångssätt: https://github.com/funcool/cats
- “Core.async” för asynkron programmering: https://github.com/clojure/core.async
