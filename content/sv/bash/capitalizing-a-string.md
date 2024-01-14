---
title:    "Bash: Att göra en sträng större"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför?
Att skriva program i Bash är ett smidigt sätt att automatisera uppgifter och arbetsflöden. Genom att kunna hantera strängar på ett effektivt sätt kan vi enkelt manipulera data och skapa användbara verktyg. Att kunna konvertera strängar till versaler är ett vanligt förfarande inom programmering och kan användas för allt från att formatera utskrifter till att hantera användardata. I denna guide kommer vi att lära oss hur man kapitaliserar en sträng i Bash.

## Hur gör man?
Att kapitalisera en sträng i Bash är enkelt och kan göras med hjälp av ett par olika metoder. Nedan följer ett par kodexempel på hur man kan göra detta:

```Bash
# Metod 1: Med hjälp av "tr" kommandot
echo "ett exempel på en sträng" | tr '[:lower:]' '[:upper:]'
# Output: ETT EXEMPEL PÅ EN STRÄNG

# Metod 2: Med hjälp av "awk" kommandot
echo "ett exempel på en sträng" | awk '{print toupper($0)}'
# Output: ETT EXEMPEL PÅ EN STRÄNG

# Metod 3: Med hjälp av "sed" kommandot
echo "ett exempel på en sträng" | sed 's/\(.*\)/\U\1/'
# Output: ETT EXEMPEL PÅ EN STRÄNG
```

Som vi kan se i exemplen ovan används olika kommandon för att åstadkomma samma resultat. Kommandona "tr", "awk" och "sed" har alla olika användningsområden, men i detta fall kan de alla användas för att konvertera en sträng till versaler. Det är viktigt att märka att de olika metoderna kan ge olika resultat beroende på vilket språk som används. Till exempel kan "tr" endast hantera ASCII-bokstäver medan "awk" och "sed" kan hantera olika teckenkodningar.

## Djupdykning
Nu när vi vet hur man kapitaliserar en sträng i Bash, kan vi titta närmare på hur dessa kommandon faktiskt fungerar. "tr" kommandot står för "translate" och används för att ersätta tecken eller teckenmönster i en sträng. I vårt exempel använder vi det för att ersätta alla små bokstäver med stora bokstäver. "awk" står för "Aho, Weinberger och Kernighan" och är namnet på det programmeringsspråk som är grunden för detta kommando. Det används främst för att behandla textfiler och kan utföra en mängd olika operationer på strängar. "sed" står för "stream editor" och används för att söka och ersätta text i en fil eller ström av data enligt ett givet mönster.

Det finns många andra sätt att konvertera en sträng till versaler i Bash, och det viktigaste är att hitta en metod som fungerar bäst för ditt specifika användningsområde.

## Se även
* [Bash Manual](http://www.gnu.org/software/bash/manual/bash.html)
* [tr kommandot i GNU coreutils](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
* [awk kommandot i GNU AWK user's guide](https://www.gnu.org/software/gawk/manual/gawk.html)
* [sed kommandot i GNU sed user's manual](https://www.gnu.org/software/sed/manual/sed.html)

Förhoppningsvis har denna guide gett dig en förståelse för hur man kapitaliserar en sträng i Bash och introducerat dig till några användbara kommandon för stränghantering. Det finns många fler kommandon och tekniker att utforska, så fortsätt öva och experimentera för att bli en mästare på Bash-programmering!