---
title:                "Store bokstaver i en streng"
html_title:           "Clojure: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr å gjøre den første bokstaven stor bokstav, mens de resterende bokstavene er små. Dette brukes vanligvis for å gi tydelighet og konsistens i kode, spesielt når det kommer til funksjonsnavn og variabler.

## Hvordan:
```Clojure 
(clojure.string/capitalize "clojure");
;; => "Clojure"

(clojure.string/capitalize "code is cool");
;; => "Code is cool"
```

## Dypdykk:
Å kapitalisere strenger er et vanlig konsept i programmering og brukes ofte for å følge konvensjoner og standarder. Alternativet til å bruke `capitalize` -funksjonen er å manuelt endre strengen ved å konvertere første tegn til stor bokstav ved hjelp av `string/upper-case` funksjonen. Implementasjonen av `capitalize` i Clojure er basert på Unicode-standarden, noe som betyr at den ikke bare støtter engelske bokstaver, men også internasjonale bokstaver og symboler.

## Se Også:
* [Clojure Offisiell Dokumentasjon](https://clojure.org/api/clojure.string#clojure.string/capitalize)
* [Unicode Standard](https://unicode.org/standard/standard.html)