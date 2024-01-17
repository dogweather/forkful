---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp existerar är en viktig del av programmering eftersom det hjälper till att säkerställa att ditt program fungerar korrekt. Genom att kontrollera om mappen redan finns kan du undvika att skriva över viktig data eller orsaka fel i ditt program.

## Så här gör du:
För att kontrollera om en mapp existerar i Bash kan du använda kommandot "test" tillsammans med flaggan "-d" och ange sökvägen till mappen du vill kontrollera. Om mappen existerar kommer kommandot att returnera sanning (0). Annars kommer det att returnera falskt (1).

```Bash
test -d /sökväg/till/mapp
echo $?
```

Om mappen finns kommer ovanstående kod att skriva ut "0" som betyder sanning. Om mappen inte finns kommer den istället att skriva ut "1" för falskt.

## Djupdykning:
Historiskt sett har kontroll av en mapp i Bash gjorts med hjälp av "ls" kommandot tillsammans med flaggan "-d". Detta har dock visat sig vara mindre tillförlitligt eftersom kommandot även listar innehållet i en mapp, vilket kan orsaka problem om mappen är tom.

Alternativ till att använda "test" kommandot är att använda "if" villkorsuttryck eller "stat" kommandot för att kontrollera en mapp i Bash.

## Se även:
- [Bash Getting Started Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Test Command](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html#Bash-Conditional-Expressions)