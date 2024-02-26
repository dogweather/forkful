---
date: 2024-01-26 01:09:50.937235-07:00
description: "\xC5 organisere kode i funksjoner handler om \xE5 pakke inn kodeblokker\
  \ som utf\xF8rer spesifikke oppgaver. Dette gj\xF8r at koden din blir ryddigere,\
  \ lettere \xE5\u2026"
lastmod: '2024-02-25T18:49:38.635404-07:00'
model: gpt-4-1106-preview
summary: "\xC5 organisere kode i funksjoner handler om \xE5 pakke inn kodeblokker\
  \ som utf\xF8rer spesifikke oppgaver. Dette gj\xF8r at koden din blir ryddigere,\
  \ lettere \xE5\u2026"
title: Organisering av kode i funksjoner
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å organisere kode i funksjoner handler om å pakke inn kodeblokker som utfører spesifikke oppgaver. Dette gjør at koden din blir ryddigere, lettere å vedlikeholde, og en lek for andre utviklere å lese.

## Hvordan:

Funksjoner i Clojure defineres med `defn`, etterfulgt av et navn, parametere og kropp. Her er et kjapt eksempel.

```Clojure
(defn hilsen [navn]
  (str "Hei, " navn "!"))

(hilsen "Alex") ; => "Hei, Alex!"
```

La oss si at vi ønsker å beregne arealet av et rektangel. I stedet for å rotte det hele sammen, deler vi det inn i to funksjoner:

```Clojure
(defn areal [lengde bredde]
  (* lengde bredde))

(defn skriv-areal [lengde bredde]
  (println "Arealet er:" (areal lengde bredde)))

(skriv-areal 3 4) ; => Arealet er: 12
```

## Dypdykk

I gamle dager ville kodere bare hamre all sin logikk inn i en enkelt blokk. Det var ikke pent. Så kom strukturert programmering, og funksjoner ble en ting. I Clojure er hver funksjon førsteklasses—du kan slynge dem rundt som hvilken som helst annen verdi.

Alternativer? Noen folk kan eksperimentere med multimetoder eller høyereordens funksjoner, men de er bare krydder i funksjonsgryta.

Detaljer i en funksjon: de er uforanderlige i Clojure, noe som gjør at bivirkningsrot blir mindre sannsynlig. De støtter seg tungt på rekursjon i stedet for typiske løkker, noe som passer godt sammen med språkets funksjonelle paradigmer.

## Se også

- Clojures egen guide: https://clojure.org/guides/learn/functions
- Grunnleggende om funksjonell programmering: https://www.braveclojure.com/core-functions-in-depth/
- Rich Hickeys taler: https://changelog.com/posts/rich-hickeys-greatest-hits - for innsikt i Clojures filosofi.
