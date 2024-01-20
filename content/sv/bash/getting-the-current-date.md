---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Bash-programmering: Så här får du nuvarande datum

## Vad & Varför?
Att hämta det nuvarande datumet innebär att hämta systemets verkliga tidspunkt. Programmeurs gör detta för att logga händelser, tidstämpla filändringar, eller för att schemalägga uppgifter.

## Så här gör du:
Här är några exempel på hur du får datum och tid i Bash. Prova det.

```Bash
# Få det nuvarande datumet
datum=$(date)
echo "Det nuvarande datumet är: $datum"

# Få nuvarande datum och tid
datetime=$(date '+%Y/%m/%d_%H:%M:%S')
echo "Det nuvarande datumet och tiden är: $datetime"
```

När du exekverar koden ovan kommer din terminal att visa nuvarande datum och tid.

## På Djupet
Historiskt har systemtiden varit avgörande för att få maskiner att fungera tillsammans. Den POSIX-definierade `date` kommandot har varit standard för Unix-liknande operativsystem sedan 80-talet.

Det finns alternativ till `date`, såsom `strftime` i C/C++. Det är mjukvara som bestämmer vilket alternativ som är mest lämpligt.

`date`-kommandot i sig är ganska rakt på sak. Det ringer till systemklockan och formaterar output till en sträng. Du kan skräddarsy formatet genom att använda olika format, som i det andra exemplet ovan.

## Se Även 
Mer information om `date` och relaterade ämnen:

1. [Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
3. [Datetime i Bash - Stack Overflow](https://stackoverflow.com/questions/1401482/yyyy-mm-dd-format-date-in-shell-script)

Lycka till med dina Bash-programmeringsäventyr!