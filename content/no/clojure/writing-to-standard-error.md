---
title:    "Clojure: Skriver til standardfeil"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Hvorfor

Når vi koder i Clojure, er det viktig å forstå hvordan man håndterer feil og unntak i programmet. Skriving til standard feil-output, eller standard error, er en vanlig metode for å informere om feil og utvikle feilsøkingsstrategier.

# Hvordan

For å skrive til standard error i Clojure, kan vi bruke funksjonen `println` med `System/err` som argument. For eksempel:

```Clojure
(println (System/err "Dette er en feilmelding."))
```

Output vil se slik ut:

```Clojure
java.lang.Exception Dette er en feilmelding.
```

Her blir feilmeldingen vår skrevet til standard error-output, og kan enkelt identifiseres ved å ha `java.lang.Exception` foran selve meldingen. Dette er nyttig for feilsøking, da vi kan se direkte i konsollen om det har oppstått noen uventede feil i koden vår.

# Dypdykk

Når vi skriver til standard error i Clojure, kan det også være nyttig å bruke makroen `with-out-str`. Denne makroen lar oss fange all output som normalt ville blitt skrevet til standard out, og isteden skrive det til en streng. Dette kan være nyttig når vi vil logge feilmeldinger til en fil, istedenfor å utskrive dem direkte i konsollen.

For eksempel:

```Clojure
(with-out-str (println (System/err "Dette er en feilmelding.")))
```

Her vil feilmeldingen bli fanget og lagret i en streng, som vi kan håndtere på ønsket måte.

# Se Også

- [Offisiell Clojure Dokumentasjon](https://clojure.org/reference/java_interop)
- [Why and How to Log Errors in Your Clojure Applications](https://www.luminusweb.net/docs/logging.md)
- [Unleash the Power of Logging with Clojure](https://medium.com/@osteele/unleash-the-power-of-logging-with-clojure-8f4c66c9226a)