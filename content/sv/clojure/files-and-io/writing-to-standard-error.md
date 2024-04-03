---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:27.283934-07:00
description: "Att skriva till standardfel (stderr) handlar om att dirigera felmeddelanden\
  \ och diagnostik till stderr-str\xF6mmen, separat fr\xE5n standardutdata (stdout).\u2026"
lastmod: '2024-03-13T22:44:37.541379-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) handlar om att dirigera felmeddelanden\
  \ och diagnostik till stderr-str\xF6mmen, separat fr\xE5n standardutdata (stdout)."
title: Skriva till standardfel
weight: 25
---

## Vad & Varför?
Att skriva till standardfel (stderr) handlar om att dirigera felmeddelanden och diagnostik till stderr-strömmen, separat från standardutdata (stdout). Programmerare gör detta för att skilja vanlig programutdata från felmeddelanden, vilket möjliggör mer effektiv felsökning och loggning.

## Hur man gör:
I Clojure kan du skriva till stderr med hjälp av `*err*`-strömmen. Här är ett grundläggande exempel:

```clojure
(.write *err* "Det här är ett felmeddelande.\n")
```

Observera att efter att du har skrivit ett meddelande, bör du tömma strömmen för att säkerställa att meddelandet omedelbart blir utskrivet:

```clojure
(flush)
```

Exempelutdata till stderr:
```
Det här är ett felmeddelande.
```

Om du hanterar undantag, kanske du vill skriva ut stackspårningar till stderr. Använd `printStackTrace` för detta:

```clojure
(try
  ;; Kod som kanske kastar ett undantag
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

För mer strukturerad felloggning kan tredjepartsbibliotek som `timbre` konfigureras för att logga till stderr. Här är en grundläggande inställning och användning:

Först, lägg till `timbre` i dina beroenden. Konfigurera sedan den för att använda stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Inaktivera stdout-loggning
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Inaktivera filloggning
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Aktivera stderr för fel

(timbre/error "Ett fel inträffade vid behandling av din förfrågan.")
```

Detta kommer att dirigera meddelanden på felnivå till stderr, och göra dem distinkta från den standardmässiga applikationsutdatan.
