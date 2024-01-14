---
title:                "Bash: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är en viktig del av Bash-programmering. Genom att skriva till standardfel istället för standardutmatning, kan du tydligt separera utmatningen från eventuella felmeddelanden som kan uppstå under körningen av ditt skript. Detta gör det enklare att felsöka och förbättra din kod.

## Hur man gör

För att skriva till standardfel i Bash, använder du kommandot "echo" tillsammans med ">&2" för att skicka utmatningen till standardfel istället för standardutmatning. Här är ett enkelt exempel:

```Bash
echo "Hej, detta är ett felmeddelande" >&2
```

Detta kommer att skriva ut meddelandet "Hej, detta är ett felmeddelande" till standardfel istället för att skriva ut det till terminalen. Det är viktigt att inkludera ">&2" annars kommer det att skriva ut meddelandet till standardutmatning istället.

Du kan också kombinera detta med andra Bash-kommandon för att skapa mer komplexa felmeddelanden. Till exempel:

```Bash
if [ ! -f "minfil.txt" ]; then
  echo "Filen minfil.txt finns inte" >&2
  exit 1
else
  echo "Filen minfil.txt hittades" >&2
  # fortsätt med resten av koden
fi
```

Här kontrollerar vi om filen "minfil.txt" finns och om den inte gör det skriver vi ut ett felmeddelande till standardfel och avslutar sedan skriptet med en exit-kod på 1.

## Djupdykning

Att skriva till standardfel ger en tydlig separation mellan utmatning och felmeddelanden, men det är också viktigt att förstå hur du kan hantera dessa felmeddelanden. Du kan använda "2>" operatören för att omdirigera standardfel till en specifik fil eller ansluta det till ett annat kommando. Till exempel:

```Bash
minskript.sh 2> felloggar.txt
```

Detta kommer att köra skriptet "minskript.sh" och omdirigera eventuella felmeddelanden till filen "felloggar.txt" istället för att skriva ut dem till terminalen. Du kan också använda "2>&1" för att kombinera standardfel med standardutmatning, vilket kan vara användbart för att få en komplett översikt av hur ditt skript körs.

## Se även

- [Bash Guide for Beginners (kapitel 13)](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_12_02.html)
- [Bash scripting tutorial (kapitel 8)](https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php#stderr)
- [BASH Programming - Introduction HOW-TO (sektion 5.3)](http://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO-5.html)