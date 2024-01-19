---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att starta ett nytt projekt i programmering innebär att skapa en ny, unik kodrad för att bygga en applikation eller tjänst från grunden. Programmerare gör detta för att utveckla programvara som uppfyller specifika behov eller för att utforska nya idéer.

## Hur man gör:
I Bash kan du skapa en ny katalog för ditt projekt och navigera till den med följande kommandon:

```Bash
mkdir mitt_projekt
cd mitt_projekt
```

Skapa en ny fil vid namn `main.sh` och sätt tillräckliga rättigheter för att köra den:

```Bash
touch main.sh
chmod +x main.sh
```

Öppna filen med en textredigerare, exempelvis `nano`, och skriv ditt första skript:

```Bash
nano main.sh
```

I `nano` kan vi till exempel skriva:

```Bash
#!/bin/bash
echo "Hej Världen!"
```

Spara (`CTRL+O`) och avsluta (`CTRL+X`) `nano`. Kör nu ditt skript:

```Bash
./main.sh
```

Du bör se "Hej Världen!" skrivet till terminalen.

## Fördjupning
Bash (Bourne Again Shell) skapades 1989 som ett fritt alternativ till de då existerande skal som UNIX-distributioner använde. Sedan dess har det blivit en standard för *nix baserade system.

Ett alternativ till Bash är `sh` (Bourne Shell), den ursprungliga shellen för UNIX. Men Bash erbjuder mer funktionalitet som te.x. arraystöd.

När du startar ett nytt projekt i Bash lägger du grunden för koden. Det är här du organiserar filer och kataloger för att förbereda utvecklingsmiljön. Det här sättet att starta ett projekt stöds av många andra programmeringsspråk eller skriptspråk som Python, JavaScript, etc.

## Se även:
- Bash reference docs: https://www.gnu.org/software/bash/manual/bash.html
- Learn Bash the hard way: https://learnbashthehardway.com/
- Basics of Shell programming: https://opensource.com/article/19/10/programming-bash-basic-operational-tools
- Linux Command tutorial: https://ryanstutorials.net/linuxtutorial/