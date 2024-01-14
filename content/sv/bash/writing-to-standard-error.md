---
title:                "Bash: Skriva till standardfel"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av Bash-programmering eftersom det hjälper till att hantera felmeddelanden och felsökning. Genom att skriva till standard error kan du skilja mellan felmeddelanden och vanlig utdata, vilket gör det lättare att identifiera problem i din kod.

## Så här gör du

För att skriva till standard error i Bash, används kommandot `echo` tillsammans med flaggan `>&2`. Detta kommer att omdirigera utdata till standard error istället för standard utdata. Här är ett enkelt exempel:

```Bash
echo "Detta är ett felmeddelande" >&2
```
Detta kommer att skriva ut "Detta är ett felmeddelande" till standard error och det kommer att bli markerat med röd text i konsolen. Detta hjälper dig att enkelt skilja mellan vanlig utdata och felmeddelanden.

Du kan också använda `echo` tillsammans med andra kommandon för att skriva till standard error. Till exempel:

```Bash
ls not_a_directory 2>&1
```

I det här fallet kommer `ls` kommandot att försöka lista ut innehållet i en mapp som inte finns och skriva ut felmeddelandet till standard error. `2>&1` betyder att all utdata, både standard out och standard error, kommer att omdirigeras till samma ställe, vilket i det här fallet är tillbaka till konsolen.

## Djupdykning

Skriva till standard error är ett sätt att hantera felmeddelanden, men det finns också andra sätt att hantera dem. Till exempel kan du använda kommandot `trap` för att fånga felmeddelanden och hantera dem på ett mer strukturerat sätt.

För att lära dig mer om hantering av felmeddelanden i Bash, rekommenderas att läsa på om `trap` kommandot och dess möjligheter. Det är ett kraftfullt verktyg som kan hjälpa dig att effektivt hantera fel i din kod.

## Se även

Här är några användbara länkar för att lära dig mer om skriver till standard error i Bash:

- [Bash tutorial: Writing to standard error](https://linuxize.com/post/bash-write-to-file-and-stdout-stderr/#writing-to-standard-error)
- [Trap command in Bash](https://www.tecmint.com/using-bash-trap-commands/)
- [Working with standard error in Bash](https://opensource.com/article/19/6/working-standard-error-bash)

Lycka till med att skriva till standard error i dina Bash-program!