---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Feilsøking med utskrift i Clojure

## Hva & Hvorfor?
Feilsøking med utskrift, eller debug-output, er kunsten å bruke programmeringsspråkets utskriftsinstrukser for å kontrollere og verifisere programkoden under kjøringen. Det gjør det mulig for utviklere å se på direkte hva som foregår i programmet, noe som hjelper med å identifisere og løse problemer.

## Hvordan gjøre det:
Den grunnleggende ideen om debug-output er å sette inn print-funksjonen der du trenger å se verdiene av variabler eller flyten av programmet. La oss se noen enkle eksempler med Clojure.

```Clojure
(defn summere [x y]
  (println "x: " x)
  (println "y: " y)
  (let [sum (+ x y)]
    (println "sum: " sum)
    sum))
    
(summere 2 3)
```

Når du kjører dette eksemplet, vil du se følgende output:

```Clojure
x: 2
y: 3
sum: 5
```

Her brukes `println`-funksjonen i Clojure for å vise statusen på variablene `x`, `y`, og `sum` under kjøringen.

## Dypdykk
Bruken av utskrift for feilsøking er nesten like gammel som programmering selv. Selv om mange moderne verktøy kommer med kraftige feilsøkingsmekanismer, fortsetter utskrift å være en grunnleggende og effektiv metode. 

En alternativ metode i Clojure er bruk av biblioteket `tools.logging` som gir mer kontroll over logging-mekanismer, inkludert loggnivåer og muligheten til å skrive til fil. 

Clojure's `println` funksjon skriver til standard utdatastrøm (som vanligvis er terminalen hvor programmet kjøres). Dette kan omdirigeres til en fil eller noe annet om nødvendig.

## Se også
For å bli mer dyktig i feilsøking av Clojure kode anbefaler jeg følgende ressurser:
* [Debugging with the Scientific Method](https://www.youtube.com/watch?v=FihU5JxmnBg) - fra av Clojure's skaper, Rich Hickey.