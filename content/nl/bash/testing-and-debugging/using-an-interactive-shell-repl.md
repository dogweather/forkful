---
title:                "Het gebruik van een interactieve shell (REPL)"
aliases:
- nl/bash/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:00.013842-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
REPL staat voor Read-Eval-Print Loop, een eenvoudige, interactieve computerprogrammeeromgeving. Programmeurs gebruiken het om snel code te schrijven en te testen, te experimenteren met syntaxis en programmeerconcepten te leren zonder de overhead van het creëren en uitvoeren van volledige applicaties.

## Hoe te:
In Bash is je terminal in wezen een REPL. Je typt een commando; het leest het, evalueert het, drukt het resultaat af en gaat terug in afwachting van je volgende commando. Hier is een voorbeeld van het gebruik van Bash als een REPL:

```Bash
$ echo "Hallo, Wereld!"
Hallo, Wereld!
$ x=$((6 * 7))
$ echo $x
42
```

Je invoer volgt op de `$` prompt, met de uitvoer afgedrukt op de volgende regel. Eenvoudig, toch?

## Diepgaand
Bash, kort voor Bourne Again SHell, is de standaardshell op veel Unix-gebaseerde systemen. Het is een upgrade van de originele Bourne-shell, gebouwd in de late jaren 1970. Hoewel Bash een krachtig scriptgereedschap is, stelt de interactieve modus je in staat commando's regel voor regel uit te voeren.

Bij het overwegen van alternatieven, heb je de Python REPL (typ gewoon `python` in je terminal), Node.js (met `node`), en IPython, een verbeterde interactieve Python-shell. Elke taal heeft de neiging zijn eigen REPL-implementatie te hebben.

Onder de motorkap zijn REPL's lussen die je invoer (commando's of code) parseren, het uitvoeren, en het resultaat terugsturen naar stdout (je scherm), vaak gebruikmakend van de taalinterpreter direct. Deze directheid van feedback is uitstekend voor leren en prototyping.

## Zie Ook
- [Officiële GNU Bash-documentatie](https://gnu.org/software/bash/manual/bash.html)
- [Leer Shell Interactieve tutorial](https://www.learnshell.org/)
- [IPython Officiële Website](https://ipython.org/)
- [REPL.it](https://replit.com/): Een multi-taal online REPL (Niet alleen Bash!)
