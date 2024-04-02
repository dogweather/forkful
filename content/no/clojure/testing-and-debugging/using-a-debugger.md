---
date: 2024-01-26 03:48:42.513574-07:00
description: "Clojure st\xF8tter seg p\xE5 Java Virtual Machine (JVM), s\xE5 mye av\
  \ feils\xF8kingen skjer med Java-verkt\xF8y. Et slikt verkt\xF8y er `CIDER`, en\
  \ kraftpakkepakke for\u2026"
lastmod: '2024-03-13T22:44:40.406720-06:00'
model: gpt-4-0125-preview
summary: "Clojure st\xF8tter seg p\xE5 Java Virtual Machine (JVM), s\xE5 mye av feils\xF8\
  kingen skjer med Java-verkt\xF8y. Et slikt verkt\xF8y er `CIDER`, en kraftpakkepakke\
  \ for\u2026"
title: "\xC5 bruke en feils\xF8ker"
weight: 35
---

## Hvordan:
Clojure støtter seg på Java Virtual Machine (JVM), så mye av feilsøkingen skjer med Java-verktøy. Et slikt verktøy er `CIDER`, en kraftpakkepakke for Clojure-utvikling i Emacs, som har solide feilsøkingskapasiteter. La oss dykke inn:

```clojure
;; Først, koble til et Clojure-prosjekt innen Emacs ved å bruke CIDER
M-x cider-jack-in

;; Sett et brytepunkt
;; Naviger til linjen i din Clojure-kode du ønsker å inspisere og
;; trykk "C-c M-b" eller utfør:
M-x cider-debug-defun-at-point

;; Når koden kjører, vil du treffe på brytepunktet. CIDER vil gi deg valg med:
;; 1. n for å gå til neste logiske skritt i utførelsen,
;; 2. c for å fortsette utførelsen til neste brytepunkt,
;; 3. q for å avslutte feilsøkingen.

;; Inspiser lokale variabler ved brytepunkt
;; Mens du er ved et brytepunkt, skriv:
locals

;; Du vil se en liste av lokale variabler og deres verdier skrevet i minibufferen.
```
Eksempel på utskrift kan se slik ut:
```clojure
{:x 10, :y 20, :result 200}
```

## Dypdykk
Debuggeren er et verktøy så gammelt som åsene i databehandlingsbegreper. Uttrykket "bug" ble myntet tilbake i de tidlige dager av databehandling når et faktisk insekt forårsaket en feil ved å kortslutte en krets i en maskin.

Selv om `CIDER` er flott for Emacs-entusiaster, finnes det alternativer for Clojure-feilsøking. For eksempel, å bruke IntelliJ med Cursive-plugin kan gi en mer GUI-drevet feilsøkingsopplevelse. Pluss, du kan bruke innebygd Leiningen eller tools.deps for å kontrollere prosessflyten når du feilsøker.

Under hetten manipulerer disse debuggerne ofte bytecode, utfører evalueringer i dedikerte nREPL økter, og tilbyr stakksporinspeksjon. De utnytter de underliggende JVM-kapasitetene ved å tappe inn i rikdommen av Javas feilsøkingsrammeverker.

## Se også
- [CIDER Debugger Dokumentasjon](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive Debugger](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen for automatisering og feilsøking](https://leiningen.org/)
- [tools.deps.alpha for mer kontroll](https://github.com/clojure/tools.deps.alpha)
