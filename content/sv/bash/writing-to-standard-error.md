---
title:    "Bash: Skriva till standardfel"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Bash-programmering är en vanlig del av många utvecklares vardag, men en funktion som kanske inte får lika mycket uppmärksamhet är skrivning till standard error (stderr). Att kunna skriva till stderr är dock en viktig del av Bash-programmering och kan hjälpa till att felsöka och förbättra koden. I denna bloggpost kommer vi att utforska varför det är viktigt att kunna skriva till stderr och hur man gör det.

## Så här gör man

För att skriva till stderr i Bash, använder vi kommandot `>&2` efter kommandot eller koden som vi vill skriva ut. Låt oss till exempel säga att vi vill skriva ut ett felmeddelande när ett kommando misslyckas. Vi kan då använda följande kod:

```Bash
ls non-existent-file 2>&1
```

Detta kommer att skriva ut felmeddelandet till stderr istället för stdout (standard output). Om vi inte hade använt `>&2`, skulle felmeddelandet ha skrivits ut till stdout som standard.

En annan användbar funktion för att skriva till stderr är att använda `echo` kommandot tillsammans med redirect-symbolen `&>`. Detta gör att allt som skrivs ut av echo-kommandot hamnar i stderr istället för stdout. Till exempel:

```Bash
echo "Detta är ett felmeddelande" &>2
```

Detta kommer att skriva ut "Detta är ett felmeddelande" till stderr.

## Djupdykning

Skrivning till stderr är en viktig del av felsökning i Bash-programmering. Genom att skriva felmeddelanden till stderr istället för stdout, kan vi enkelt skilja output från felmeddelanden och lättare identifiera problem i vår kod. Det hjälper också till att optimera koden genom att hålla output och felmeddelanden separerade.

En annan användning för att skriva till stderr är när vi vill skriva ut debug-information. Istället för att ständigt behöva kommentera och ta bort kodbaserad debugging, kan vi enkelt skriva ut debug-information till stderr och sedan enkelt stänga av det när vi inte längre behöver det.

## Se även

- [Bash Guide for Beginners (Swedish)](https://linuxkurs.ch/pub/docs/bash-guide-for-beginners.pdf)
- [The Linux Command Line (Swedish)](https://www.linux-kurs.se/wp/wp-content/uploads/2018/09/cli.pdf)
- [Official Bash Documentation](https://www.gnu.org/software/bash/manual/bash)