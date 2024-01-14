---
title:    "Bash: Läsa en textfil"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Att läsa textfiler är en vanlig uppgift inom programmering och kan användas för att läsa och bearbeta data från olika källor. Det är också ett viktigt steg i att automatisera uppgifter och skapa skript för att hantera stora mängder data.

## Hur man gör

För att läsa en textfil i Bash, kan du använda kommandot "cat" följt av filnamnet. Till exempel:

```Bash
cat min_textfil.txt
```

Detta kommando kommer att skriva ut innehållet i textfilen till terminalen.

Om du vill spara innehållet i en textfil till en variabel i Bash, kan du använda följande syntax:

```Bash
variabel=`cat min_textfil.txt`
```

Du kan också använda "while loop" för att läsa igenom en textfil rad för rad. Till exempel:

```Bash
while read rad; do
  echo "$rad"
done < min_textfil.txt
```

Detta kommando kommer att skriva ut varje rad i textfilen till terminalen. Det är också viktigt att notera att "\$rad" avgränsar varje rad och att "<" anger att läsningen ska ske från textfilen.

## Djupdykning

När du läser en textfil i Bash, är det viktigt att förstå att filen läses rad för rad. Detta betyder att om textfilen innehåller flera kolumner eller fält, måste du använda lämpliga verktyg för att bearbeta datan. Till exempel kan du använda kommandot "cut" för att välja specifika kolumner eller "grep" för att filtrera efter vissa mönster.

Det är också bra att veta att Bash läser textfiler från början till slut, vilket betyder att om du vill läsa en specifik rad eller hoppa till en viss plats, måste du använda andra kommandon som "sed" eller "head" tillsammans med "cat".

## Se också

Här nedanför hittar du några användbara länkar för att lära dig mer om att läsa textfiler i Bash:

- [Cat: A Unix Command for Viewing File Contents](https://www.lifewire.com/cat-linux-command-4090235)
- [How to Read a File Line By Line](https://bash.cyberciti.biz/guide/Reads_from_the_file_line-by-line_(while_loop))
- [Working with CSV files in Bash](https://opensource.com/article/18/5/working-csv-files-bash)
- [Text processing commands in Linux](https://linuxhint.com/text-processing-linux/)