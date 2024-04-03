---
date: 2024-01-26 04:11:18.245909-07:00
description: "REPL st\xE5r for Read-Eval-Print Loop, et enkelt, interaktivt dataprogrammeringsmilj\xF8\
  . Kodeutviklere bruker det til raskt \xE5 skrive og teste kode,\u2026"
lastmod: '2024-03-13T22:44:40.976504-06:00'
model: gpt-4-0125-preview
summary: "REPL st\xE5r for Read-Eval-Print Loop, et enkelt, interaktivt dataprogrammeringsmilj\xF8\
  ."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hvordan:
I Bash er terminalen din i bunn og grunn en REPL. Du skriver en kommando; den leser den, evaluerer den, skriver ut resultatet og looper tilbake i påvente av din neste kommando. Her er et eksempel på bruk av Bash som en REPL:

```Bash
$ echo "Hello, World!"
Hello, World!
$ x=$((6 * 7))
$ echo $x
42
```

Inntastingen din følger etter `$ ` prompten, med resultatet skrevet ut på neste linje. Enkelt, ikke sant?

## Dypdykk
Bash, forkortelse for Bourne Again SHell, er standard shell på mange Unix-baserte systemer. Det er en oppgradering av det opprinnelige Bourne shell, bygget på slutten av 1970-tallet. Selv om Bash er et kraftig skriptverktøy, lar dens interaktive modus deg utføre kommandoer linje for linje.

Når du vurderer alternativer, har du Python REPL (skriv bare `python` i terminalen din), Node.js (med `node`), og IPython, en forbedret interaktiv Python-shell. Hvert språk har som regel sin egen REPL-implementasjon.

Under overflaten er REPL-er løkker som parser inndataen din (kommandoer eller kode), kjører den, og returnerer resultatet til stdout (skjermen din), ofte ved å bruke språkets tolker direkte. Denne umiddelbarheten av tilbakemelding er utmerket for læring og prototyping.

## Se også
- [Offisiell GNU Bash-dokumentasjon](https://gnu.org/software/bash/manual/bash.html)
- [Learn Shell interaktiv tutorial](https://www.learnshell.org/)
- [IPython offisielle nettsted](https://ipython.org/)
- [REPL.it](https://replit.com/): En flerspråklig online REPL (Ikke bare Bash!)
