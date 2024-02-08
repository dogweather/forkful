---
title:                "Skriving til standardfeil"
aliases:
- no/clojure/writing-to-standard-error.md
date:                  2024-02-03T19:32:47.330594-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å skrive til standardfeil (stderr) handler om å styre feilmeldinger og diagnostikk til stderr-strømmen, separat fra standard utgang (stdout). Programmerere gjør dette for å skille vanlig programutgang fra feilmeldinger, noe som tillater mer effektiv feilsøking og logging.

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
