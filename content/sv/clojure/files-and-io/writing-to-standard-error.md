---
title:                "Skriva till standardfel"
date:                  2024-02-03T19:33:27.283934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriva till standardfel"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
