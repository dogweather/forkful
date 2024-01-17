---
title:                "Utskrift av feilsøkingsmeldinger"
html_title:           "Clojure: Utskrift av feilsøkingsmeldinger"
simple_title:         "Utskrift av feilsøkingsmeldinger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Printing debug output er en måte for programmerere å få informasjon om hvordan et program kjører. Dette kan hjelpe dem med å finne og feilsøke problemer i koden. Det er spesielt nyttig når man utvikler stort og komplekst programvare.


# Hvordan:
```Clojure
(defn hello-world []
  (println "Hello, world!"))

(hello-world)
``` 
Resultat:
`Hello, world!`

I dette eksempelet, bruker vi `println` funksjonen til å skrive ut en tekststreng til konsollen. Dette vil bli brukt til å bekrefte at funksjonen `hello-world` blir kalt og kjører som forventet. Det er viktig å merke seg at vi kun bruker print-funksjoner for å utskrive debug informasjon, og bør fjerne dem fra koden når vi har løst problemet.

# Dykk dypere:
Historisk sett, brukte programvareutviklere ofte `println` eller lignende funksjoner for å få debug informasjon. Dette ble ofte sett på som upresist og ineffektivt. En mer moderne tilnærming er å bruke debug biblioteker som `clojure.tools.logging` og `timbre`, som gir bedre kontroll over utskriftsformatering og mer robuste alternativer for debugging.

En alternativ metode for å få debug informasjon er å bruke en interaktiv debugger, som for eksempel ` CIDER` eller `Calva`. Dette gjør det mulig å sette breakpoints i koden og stegvis debugge koden.

Når det kommer til implementasjon av utskriftsfunksjoner, bør man være forsiktig med å bruke dem for ofte, da det kan føre til unødvendig bloat i koden. Det kan være lurt å bruke betinget utskrift, slik at utskriftskallene kun blir kalt når et bestemt kriterium er oppfylt.

# Se også:
- [Clojure dokumentasjon: Debugging](https://clojure.org/guides/debugging)
- [Clojure Cookbook: Debugging](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/03_language/3-05_debugging.asciidoc)
- [Calva: Interactive Clojure programming](https://calva.io/)