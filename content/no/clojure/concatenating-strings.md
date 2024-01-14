---
title:                "Clojure: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

Hvorfor: 

Å kombinere strenger er en viktig del av programmering, spesielt i Clojure. Det lar deg bygge mer dynamiske og tilpassede tekstuttrykk, som kan være nyttige for alt fra å lage brukergrensesnitt til å håndtere store datasett. 

Slik gjør du det:

```Clojure

(println "Hei," "verden!")
=> Hei, verden!

(str "Alder:" 25)
=> Alder: 25

(str "Navn: " "Ole" " " "Nordmann" " | " "Alder: " 35)
=> Navn: Ole Nordmann | Alder: 35
``` 
Dypdykk:

Når du kombinerer strenger, må du også håndtere forskjellige typer data som kan være involvert. Clojure har en nyttig funksjon kalt "str", som konverterer alle data til strenger før den kombinerer dem. Dette betyr at du kan bruke denne funksjonen med flere typer data, som tall, booleans, lister og kart. Du kan også bruke operatorer som "+" eller "format" for å gjøre mer komplekse kombinasjoner av tekst. 

Se også: 

- Offisiell Clojure dokumentasjon om å kombinere strenger: https://clojure.org/guides/learn/syntax#_string
- En artikkel om å håndtere strenger i Clojure: https://purelyfunctional.tv/guide/clojure-strings/
- En diskusjon om forskjellen mellom "str" og "format": https://stackoverflow.com/questions/22089279/clojure-format-vs-str