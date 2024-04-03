---
date: 2024-01-26 04:11:24.597455-07:00
description: "Hur man g\xF6r: I Bash \xE4r din terminal i princip en REPL. Du skriver\
  \ ett kommando; den l\xE4ser det, utv\xE4rderar det, skriver ut resultatet och loopar\
  \ tillbaka\u2026"
lastmod: '2024-03-13T22:44:38.084077-06:00'
model: gpt-4-0125-preview
summary: "I Bash \xE4r din terminal i princip en REPL."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Hur man gör:
I Bash är din terminal i princip en REPL. Du skriver ett kommando; den läser det, utvärderar det, skriver ut resultatet och loopar tillbaka och väntar på ditt nästa kommando. Här är ett exempel på hur du använder Bash som en REPL:

```Bash
$ echo "Hej, världen!"
Hej, världen!
$ x=$((6 * 7))
$ echo $x
42
```

Ditt inmatning följer `$ ` prompten, med utdatat som skrivs ut på nästa rad. Enkelt, eller hur?

## Fördjupning
Bash, förkortning för Bourne Again SHell, är standardshell på många Unix-baserade system. Det är en uppgradering till det ursprungliga Bourne-shellet, byggt i slutet av 1970-talet. Även om Bash är ett kraftfullt skriptverktyg, låter dess interaktiva läge dig exekvera kommandon rad för rad.

När du överväger alternativ har du Python REPL (skriv bara `python` i din terminal), Node.js (med `node`), och IPython, ett förbättrat interaktivt Python-shell. Varje språk tenderar att ha sin egen REPL-implementering.

Under ytan är REPL:er loopar som tolkar dina inmatningar (kommandon eller kod), kör dem, och returnerar resultatet till stdout (din skärm), ofta genom att använda språkets tolk direkt. Denna omedelbarhet av feedback är utmärkt för lärande och prototypning.

## Se även
- [Officiell GNU Bash-dokumentation](https://gnu.org/software/bash/manual/bash.html)
- [Learn Shell Interactive tutorial](https://www.learnshell.org/)
- [IPython Officiell Webbsida](https://ipython.org/)
- [REPL.it](https://replit.com/): En flerspråkig online REPL (Inte bara Bash!)
