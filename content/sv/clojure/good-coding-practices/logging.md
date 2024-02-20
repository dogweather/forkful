---
date: 2024-01-26 01:02:15.486120-07:00
description: "Loggning \xE4r i grunden mjukvarans motsvarighet till ett skeppsloggbok;\
  \ det \xE4r ett s\xE4tt att registrera h\xE4ndelser som intr\xE4ffar medan en applikation\
  \ k\xF6rs.\u2026"
lastmod: 2024-02-19 22:04:56.775448
model: gpt-4-1106-preview
summary: "Loggning \xE4r i grunden mjukvarans motsvarighet till ett skeppsloggbok;\
  \ det \xE4r ett s\xE4tt att registrera h\xE4ndelser som intr\xE4ffar medan en applikation\
  \ k\xF6rs.\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är i grunden mjukvarans motsvarighet till ett skeppsloggbok; det är ett sätt att registrera händelser som inträffar medan en applikation körs. Programmerare gör detta för att hålla reda på dessa händelser för felsökning, revisionsspår, eller för att få insikter om hur ett system beter sig i produktion.

## Hur man gör:
Clojure lutar sig mot Javas loggningsfaciliteter, men du kan använda dem på ett mer idiomatiskt Clojure-sätt. Låt oss titta på hur du kanske använder `clojure.tools.logging`, som tillhandahåller en enkel abstraktion över flera loggningsramverk:

Först, lägg till ett beroende för `clojure.tools.logging` och en loggningsimplementation som `log4j` i din `project.clj`:

```clojure
:dependencies [[org.clojure/clojure "1.10.3"]
               [org.clojure/tools.logging "1.1.0"]
               [log4j/log4j "1.2.17"]]
```

Nu, låt oss logga några meddelanden:

```clojure
(require '[clojure.tools.logging :as log])

(defn compute-answer-to-everything []
  (log/debug "Påbörjar intensiv beräkning...")
  (Thread/sleep 3000) ; Simulerar en lång beräkning
  (log/info "Beräkningen klar. Svaret är 42.")
  42)

(compute-answer-to-everything)
```
Outputen kommer inte att visa `DEBUG`-meddelanden som standard, eftersom loggnivåerna typiskt sett är inställda på `INFO`:

```
INFO  [ditt-namnrymd] - Beräkningen klar. Svaret är 42.
```

Du kan konfigurera loggnivåerna och appenders i en `log4j.properties`-fil för att få mer utförlig output vid behov.

## Djupdykning
Clojures `clojure.tools.logging` har funnits ett tag och fungerar som en bro mellan Clojure-kod och Java-loggningsvärlden. Historiskt sett har Java gått igenom flera iterationer och bibliotek för loggning såsom Javas inbyggda loggnings-API, `log4j`, `slf4j` och `logback`.

I Clojure kan du, medan du direkt kan använda Javas loggningsramverk, `clojure.tools.logging` upptäcker och delegerar till vilket loggningsramverk som än finns i din klassväg, vilket räddar dig från att vara tätt kopplad till en specifik implementering. Detta kan hjälpa till att hålla din Clojure-kod mer portabel och modulär.

Alternativ till `clojure.tools.logging` inom Clojure-ekosystemet inkluderar bibliotek som `timbre`, som är ett rent Clojure-loggningsbibliotek med funktioner som loggrotation, filtrering och asynkron loggning direkt ur lådan.

Implementeringsdetaljer är avgörande när det gäller loggning i en multi-trådad miljö som Clojure. Här ger oföränderlighet och sidoeffektshantering distinkta fördelar. Loggning, som en sidoeffekt, bör hanteras med omsorg för att undvika prestandaflaskhalsar och säkerställa trådsäkerhet, vilket de flesta Java-loggningsramverk redan tar hand om. 

Slutligen, överväg strukturerad loggning, där loggar skrivs som strukturerad data (som JSON). Det kan vara extremt användbart för senare analys och bearbetning, speciellt när man hanterar storskaliga distribuerade system.

## Se också
Om du är sugen på mer, överväg att kolla in dessa resurser:

- Clojure Tools Logging dokumentation: https://github.com/clojure/tools.logging
- Timbre, ett Clojure-loggningsbibliotek: https://github.com/ptaoussanis/timbre
- Konfigurera Log4J i Clojure: http://clojure-doc.org/articles/tutorials/logging_with_log4j.html
- Logback Manual för avancerade inställningar: http://logback.qos.ch/manual/
- En guide om strukturerad loggning i Clojure: https://corfield.org/blog/2020/04/28/structured-logging/
