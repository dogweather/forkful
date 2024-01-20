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

## Vad och Varför?
Att verifiera om en katalog existerar i Bash sammanhang innebär att kontrollera om en särskild mapp finns i systemet innan en operation utförs på den. Det är viktigt för att förhindra fel, som kan orsakas av att försöka kopiera filer in i en icke-existerande katalog.

## Så här gör du:
Att kolla upp om en katalog finns i Bash är lätt. Se koden nedan:
```Bash
if [ -d "$DIRECTORY" ]; then
  echo "Katalogen existerar."
else
  echo "Katalogen finns inte."
fi
```
Om `DIRECTORY` finns, kommer skriptet att skriva ut "Katalogen existerar". Om det inte gör det, visar skriptet "Katalogen finns inte".

## Djupgående:
Detta kommando går tillbaka till de tidiga dagarna av UNIX. Det är en del av en större grupp testkommandon som förkortas med ett enda hakparentes. Alternativa kommandon, som `test` och `[[...]]`, bjuder också på mer avancerade syntaxer och funktioner. Detta inkluderar strängjämförelse, regex matchning, och mer.

För att komma ihåg det lättare, tänk på -d som "directory exists".

## Se även:
- `man test`: UNIX manualen för testkommandon. [Länk här](http://man7.org/linux/man-pages/man1/test.1.html).
- `man bash`: Den fullständiga manualen för Bash. [Länk här](http://man7.org/linux/man-pages/man1/bash.1.html).
- Bash Scripting Guide: En helfruktig guide till Bash programmering. [Länk här](https://tldp.org/LDP/Bash-Beginners-Guide/html/Bash-Beginners-Guide.html).