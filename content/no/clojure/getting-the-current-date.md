---
title:    "Clojure: Hente nåværende dato"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor

Å få dagens dato kan være en essensiell del av mange programmeringsprosjekter. Enten det er for å holde styr på tidssensitive data eller for å generere rapporter basert på dags dato, så er det viktig å kunne få tilgang til dagens dato i et program på en enkel og nøyaktig måte.

# Hvordan

Det er flere måter å få dagens dato i Clojure. En måte å gjøre det på er ved å bruke funksjonen `java.util.Date`. Ved å importere denne i namespace, kan du bruke funksjonen `now` for å få dagens dato som en instans av `java.util.Date`:

```Clojure
(ns min-prosjekt.core
  (:require [java.util.Date :as date]))

(def dagsdato (date/now))
```

Dette vil returnere en instans av `java.util.Date` som du deretter kan jobbe med. Hvis du ønsker å få dagens dato i en spesifikk tidsone, kan du også bruke funksjonen `now-in-time-zone`:

```Clojure
(def dagsdato (date/now-in-time-zone "Europe/Oslo"))
```

Dette vil returnere dagens dato i din lokale tidsone, i dette tilfellet i Oslo sin tidsone.

For å formatere dagens dato på en bestemt måte, kan du bruke funksjonen `java.text.SimpleDateFormat`. Dette lar deg formatere dato og klokkeslett på en enkel og fleksibel måte:

```Clojure
(defn formatert-dato [dato]
  (let [format (java.text.SimpleDateFormat. "dd.MM.yyyy")]
    (.format format dato)))

(formatert-dato dagsdato) ;; returnerer f.eks. "11.09.2021"
```

# Dypdykk

Bak kulissene bruker Clojure de samme Java-klassene og metoder for å få dagens dato som i eksemplene over. Det er derfor også mulig å bruke Java-kode direkte i Clojure ved å bruke funksjonen `clojure.lang.Reflector`.

For eksempel, hvis du ønsker å få dagens dato som en streng på formatet "YYYY-MM-DD", kan du gjøre det slik:

```Clojure
(defn dato-streng []
  (clojure.lang.Reflector/invokeInstanceMethod
    (java.util.Date.) "toString" (into-array []))) ;; => "2021-09-11"
```

Det er også verdt å nevne at det finnes flere tredjepartsbiblioteker tilgjengelig for å håndtere datoer og klokkeslett i Clojure, som f.eks. `clj-time` og `time-lib`.

# Se også

- [java.util.Date dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [java.text.SimpleDateFormat dokumentasjon](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [clojure.lang.Reflector dokumentasjon](https://clojuredocs.org/clojure.core/with-meta)
- [clj-time bibliotek](https://github.com/clj-time/clj-time)
- [time-lib bibliotek](https://github.com/andersmurphy/time-lib)