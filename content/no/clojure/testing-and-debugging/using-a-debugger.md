---
title:                "Å bruke en feilsøker"
aliases:
- /no/clojure/using-a-debugger.md
date:                  2024-01-26T03:48:42.513574-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å bruke en feilsøker"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/using-a-debugger.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å bruke en debugger betyr at du utstyrer deg selv med et forstørrelsesglass for å granske koden din. Programmerere gjør dette for å knuse feil, forstå flyt og forsikre seg om at logikken deres utfolder seg som forventet.

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
