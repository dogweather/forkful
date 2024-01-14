---
title:    "Clojure: Skriver til standardfeil"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive til standard error kan være nyttig når du trenger å logge feil eller uventet oppførsel i koden din. Det lar deg skrive til en egen utgavestrøm som skiller seg fra standard utgang, og hjelper deg med å spore ned feil og feil i koden din.

## Slik gjør du det

Å skrive til standard error i Clojure er enkelt. Du kan bruke funksjonen `println` med `>>` operatøren:

```Clojure
(println >> "Dette vil skrive til standard error")
```

Når du kjører koden din, vil teksten "Dette vil skrive til standard error" bli skrevet ut i din terminal eller konsoll, og ikke i din vanlige utgangsstrøm. Du kan også kombinere `println` med `err` funksjonen som refererer til standard error-strømmen:

```Clojure
(println (err "Dette vil også skrive til standard error"))
```

Dette vil gi samme resultat som det første eksemplet.

## Dypdykk

For å få enda mer kontroll over skriving til standard error, kan du bruke `with-out-str` og `println-str` funksjonene sammen. `with-out-str` lar deg omdirigere utgangen din til en streng, i stedet for å skrive den ut i terminalen. Deretter kan du bruke `println-str` til å skrive ut strengen til standard error-strømmen:

```Clojure
(with-out-str
  (println-str "Dette vil skrives til standard error"))
```

Dette vil også omdirigere teksten til standard error-strømmen, noe som kan være nyttig hvis du ønsker å behandle utgangen på en annen måte. 

## Se også

- Clojure dokumentasjon for standard error: https://clojure.org/reference/java_interop#_standard_error
- En tutorial om å logge i Clojure: https://lispcast.com/loggin-in-clojure/
- En artikkel om feilhåndtering i Clojure: https://www.baeldung.com/clojure-error-handling