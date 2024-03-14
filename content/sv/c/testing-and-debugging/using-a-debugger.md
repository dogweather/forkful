---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:13.500535-07:00
description: "Debuggers i C \xE4r specialiserade verktyg som g\xF6r det m\xF6jligt\
  \ f\xF6r utvecklare att stega igenom sin kod, inspektera variabler och \xF6vervaka\
  \ exekveringsfl\xF6det.\u2026"
lastmod: '2024-03-13T22:44:38.387536-06:00'
model: gpt-4-0125-preview
summary: "Debuggers i C \xE4r specialiserade verktyg som g\xF6r det m\xF6jligt f\xF6\
  r utvecklare att stega igenom sin kod, inspektera variabler och \xF6vervaka exekveringsfl\xF6\
  det.\u2026"
title: "Att anv\xE4nda en debugger"
---

{{< edit_this_page >}}

## Vad & Varför?

Debuggers i C är specialiserade verktyg som gör det möjligt för utvecklare att stega igenom sin kod, inspektera variabler och övervaka exekveringsflödet. Denna process är avgörande för att identifiera och åtgärda buggar, samt för att säkerställa att koden fungerar som förväntat.

## Hur gör man:

GDB (GNU Debugger) är den mest använda debuggern för C-programmering. Här är en kort guide om hur man använder GDB för att felsöka ett enkelt C-program.

Först, kompilera ditt C-program med flaggan `-g` för att inkludera debugginginformation:

```c
gcc -g program.c -o program
```

Starta sedan GDB med ditt kompilerade program:

```bash
gdb ./program
```

Nu kan du använda olika kommandon inom GDB för att styra dess funktion. Här är några grundläggande kommandon:

- `break`: Sätt en brytpunkt vid en angiven rad eller funktion för att pausa exekveringen.
  - Exempel: `break 10` eller `break main`
- `run`: Starta exekveringen av ditt program inom GDB.
- `next`: Exekvera nästa rad kod utan att stega in i funktioner.
- `step`: Exekvera nästa rad kod, steget in i funktioner.
- `print`: Visa värdet av en variabel.
- `continue`: Återuppta exekveringen till nästa brytpunkt.
- `quit`: Avsluta GDB.

Här är ett exempelsession som felsöker ett enkelt program:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Kompilera och starta GDB som beskrivet. Sätt en brytpunkt vid `printf`-raden med `break 5` och sedan `run`. Använd `next` för att stega igenom loopen och `print i` för att inspektera loopvariabeln.

Exempelutskrift efter att en brytpunkt satts och före den första iterationen:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Använda `print i` efter några iterationer:

```
$3 = 2
```

Detta demonstrerar undersökningen av tillståndet och flödet i ett enkelt program.

## Fördjupning

Konceptet med felsökning har utvecklats avsevärt sedan programmeringens tidiga dagar, där fysiska buggar (bokstavliga insekter) kunde orsaka problem i mekaniska datorer. Idag erbjuder debuggers som GDB sofistikerade funktioner utöver grundläggande stegning och variabelinspektion, såsom omvänd felsökning (att exekvera programmet bakåt), villkorliga brytpunkter och skriptning för automatiserade felsökningsuppgifter.

Även om GDB är kraftfullt och mycket använt, kan det vara tätt och utmanande för nybörjare. Alternativa felsökningsverktyg och IDE:er (Integrated Development Environments) som Visual Studio Code, CLion eller Eclipse erbjuder mer användarvänliga gränssnitt för felsökning av C-kod, ofta med visuella hjälpmedel och mer intuitiva kontroller. Dessa alternativ kanske inte erbjuder fullt så djup funktionalitet som GDB, men kan vara mer tillgängliga för nykomlingar till C-programmering.

Dessutom har framväxten av språkserverprotokoll och standarder för felsökning underlättat plattformsoberoende felsökningslösningar, vilket gör felsökningsupplevelsen mer konsekvent över olika verktyg och miljöer. Trots dessa framsteg är lärandet av en traditionell debugger som GDB ovärderlig insikt i exekveringen av C-program och förblir en avgörande färdighet i en utvecklares verktygslåda.
