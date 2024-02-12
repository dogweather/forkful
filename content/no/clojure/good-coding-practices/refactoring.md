---
title:                "Refaktorering"
aliases:
- no/clojure/refactoring.md
date:                  2024-01-26T01:17:27.427776-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Refaktorering er prosessen med å restrukturere eksisterende datakode uten å endre dens eksterne oppførsel, med mål om å forbedre ikke-funksjonelle attributter. Programmerere refaktorerer for å gjøre koden deres renere, mer effektiv og enklere å vedlikeholde, noe som effektivt forbedrer lesbarheten og reduserer kompleksiteten i programvaren deres.

## Hvordan:

Refaktorering i Clojure—takket være dets rene syntaks og funksjonelle paradigme—kan være utrolig greit. La oss ta for oss et vanlig scenario: iterering over samlinger. Du kan starte med en `for`-løkke, slik som dette:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Å kalle `(old-way)` vil gi oss 55, summen fra 1 til 10. Men, hei, vi kan refaktorere dette til å være mer Clojure-aktig:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Denne refaktorerte `(new-way)`-funksjonen bruker gjennomstrømmingsmakroer for å sende området direkte til `reduce`, og trimmer unødvendig fett.

## Dykking Dypt

Kunsten å refaktorere har sine røtter i de tidlige dagene av programvareutvikling, men fikk virkelig grep med Martin Fowlers banebrytende bok "Refaktorering: Forbedring av designet på eksisterende kode" utgitt i 1999. I Clojure lener refaktorering seg ofte på funksjonelle programmeringsprinsipper, og favoriserer rene funksjoner og uforanderlige datastrukturer.

Alternativer til manuell refaktorering i Clojure kan inkludere bruk av verktøy som Cursive, en populær IntelliJ IDEA-plugin, som tilbyr automatiserte refaktoreringer spesifikt for Clojure. Det er også clj-refactor, en Emacs-pakke for Clojure, som gir en rekke refaktoriseringsfunksjoner.

En utfordring som er spesiell for refaktorering i Clojure, er å håndtere tilstand og sideeffekter i et prinsipielt uforanderlig og sideeffektfritt paradigme. Forsiktig bruk av atomer, referanser, agenter og forbigående er avgjørende for å opprettholde både ytelse og korrekthet under refaktoreringer.

## Se Også

- Martin Fowlers "Refaktorering: Forbedring av designet på eksisterende kode" for de grunnleggende konseptene.
- [Clojure Docs](https://clojuredocs.org/) for spesifikke eksempler på idiomatisk Clojure-kode.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) for automatisering av refaktorering i Emacs.
- [Cursive](https://cursive-ide.com/) for IntelliJ-brukere som søker automatisert refaktoreringsassistanse.
- [Refaktorering med Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Et foredrag av Clojures skaper som, selv om det ikke handler om refaktorering per se, gir innsikt i Clojure-filosofien som kan veilede effektive refaktoreringsbeslutninger.
