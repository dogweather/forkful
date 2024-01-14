---
title:                "Bash: Utskrift av felsökningsutdata"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debuggutdata är ett användbart verktyg för att felsöka och förbättra Bash-programmeringserfarenhet. Genom att skriva ut variabler, värden och steg i ditt program kan du enkelt följa flödet och hitta eventuella fel.

## Hur man gör

Det finns flera sätt att skriva ut debuggutdata i Bash. Det enklaste sättet är att använda kommandot "echo" för att skriva ut en variabel eller ett meddelande. Till exempel:

```Bash
#!/bin/bash
namn="Emma"
echo "Hej $namn!"
```

Detta kommer att skriva ut meddelandet "Hej Emma!" när programmet körs.

För mer detaljerad debuggutdata kan du använda kommandot "set -x" för att aktivera "trace mode". Detta kommer att skriva ut varje steg i ditt program tillsammans med dess värden. Till exempel:

```Bash
#!/bin/bash
set -x
namn="Emma"
ålder=25
echo "$namn är $ålder år gammal"
```

Detta kommer att skriva ut:

```
+ namn=Emma
+ ålder=25
+ echo 'Emma är 25 år gammal'
Emma är 25 år gammal
```

Det är också möjligt att skriva ut ett meddelande för att identifiera viktiga steg i ditt program. Till exempel:

```Bash
#!/bin/bash
namn="Emma"
echo "Följande kod visar värdet av variabeln namn:"
echo "Namn: $namn"
```

Detta kommer att skriva ut:

```
Följande kod visar värdet av variabeln namn:
Namn: Emma
```

## Djupdykning

Att skriva ut debuggutdata kan också vara användbart för att kontrollera och jämföra värden. Till exempel, om du har en loop som ska öka ett nummer med ett visst värde varje gång, kan du skriva ut det ökade värdet för att se om det är korrekt.

En annan användbar funktion är att använda "printf" för att skriva ut format och värden. Detta ger dig möjlighet att kontrollera och formatera hur din debuggutdata visas. Till exempel:

```Bash
#!/bin/bash
number1=5
number2=10
summa=$(( $number1 + $number2 ))
printf "Summan av %s och %s är %s\n" "$number1" "$number2" "$summa"
```

Detta kommer att skriva ut:

```
Summan av 5 och 10 är 15
```

## Se även

- [Bash referenshandbok](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide för nybörjare](http://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [10 Bash Tips och Tricks för att spara tid och förbättra produktiviteten](https://www.tecmint.com/best-linux-bash-tips-and-tricks/)