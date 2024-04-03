---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:47.330594-07:00
description: "Hvordan: I Clojure kan du skrive til stderr ved \xE5 bruke `*err*`-str\xF8\
  mmen. Her er et grunnleggende eksempel."
lastmod: '2024-03-13T22:44:40.417972-06:00'
model: gpt-4-0125-preview
summary: "I Clojure kan du skrive til stderr ved \xE5 bruke `*err*`-str\xF8mmen."
title: Skriving til standardfeil
weight: 25
---

## Hvordan:
I Clojure kan du skrive til stderr ved å bruke `*err*`-strømmen. Her er et grunnleggende eksempel:

```clojure
(.write *err* "Dette er en feilmelding.\n")
```

Merk at etter å ha skrevet en melding, bør du tømme strømmen for å sikre at meldingen blir umiddelbart utgitt:

```clojure
(flush)
```

Eksempelutskrift til stderr:
```
Dette er en feilmelding.
```

Hvis du håndterer unntak, kan du ønske å skrive ut stackspor til stderr. Bruk `printStackTrace` for dette:

```clojure
(try
  ;; Kode som kan kaste et unntak
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

For mer strukturert feillogging, kan tredjepartsbiblioteker som `timbre` konfigureres for å logge til stderr. Her er en grunnleggende oppsett og bruk:

Først, legg til `timbre` i dine avhengigheter. Deretter konfigurerer du det til å bruke stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Deaktiver stdout-logging
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Deaktiver fillogg
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Aktiver stderr for feil

(timbre/error "En feil oppsto under behandlingen av forespørselen din.")
```

Dette vil dirigere meldinger på feilnivå til stderr, og gjøre dem ulik standard programutgang.
