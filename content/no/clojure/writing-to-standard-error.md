---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standard feil (`stderr`) er hvordan programmer melder fra om feil og advarsler. Det holder disse meldingene separate fra hoveddataflyten, og gjør det lettere å debugge og overvåke applikasjoner.

## How to:
Clojure bruker Java sin `System/err` for å skrive til `stderr`. Her er hvordan du gjør det:

```Clojure
(.println System/err "Dette er en feilmelding til stderr.")
```

Kjører du dette, får du:

```
Dette er en feilmelding til stderr.
```

En mer idiomatic måte å håndtere dette i Clojure:

```Clojure
(defn log-to-stderr [message]
  (.println System/err message))

(log-to-stderr "Oops, noe gikk galt!")
```

Dette vil også skrive til stderr:

```
Oops, noe gikk galt!
```

## Deep Dive
Før operativsystemene skilte standard utdata (`stdout`) og `stderr`, gikk all tekstutdata fra programmer til samme sted. Separasjonen tillater oss å omdirigere disse uavhengig av hverandre, for eksempel for å filtrere ut feilmeldinger til en loggfil.

I Clojure er det vanligere å bruke logging-biblioteker som `timbre` eller `log4j` istedenfor direkte kall til `System/err`, fordi bibliotekene gir mer fleksibilitet og formattering.

I bunnlinjen er skriving til `stderr` implementert av underliggende operativsystem- og språkinfrastruktur (i dette tilfellet Java), og Clojure-koden kaller bare disse funksjonene gjennom Java interoperabilitet.

## See Also
- Clojure's `timbre` logging bibliotek: [https://github.com/ptaoussanis/timbre](https://github.com/ptaoussanis/timbre)
- Oracle's Java dokumentasjon om `System/err`: [https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#err)
- Clojure's officielle guide til Java interoperabilitet: [https://clojure.org/reference/java_interop](https://clojure.org/reference/java_interop)
