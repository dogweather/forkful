---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil är den aktion där en programmerare extraherar data från en fil för att använda inom sitt script. Vi gör det för att hämta, analysera eller manipulera informationen inuti.

## Hur man gör:
```Bash
# Du kan använda 'cat' kommandot för att läsa en textfil.
cat filnamn.txt

# Du kan även använda 'less' eller 'more'.
less filnamn.txt
more filnamn.txt
```
Om din 'filnamn.txt' innehåller "Hej, Sverige!" kommer var och en av dessa kommandon att visa "Hej, Sverige!" på terminalen.

```Bash
# Om du vill läsa en textfil rad för rad, använd en while-loop:
while IFS= read -r rad; do
    echo "$rad"
done < filnamn.txt
```
Detta skript kommer att visa varje rad i 'filnamn.txt' på en ny rad i terminalen.

## Fördjupning
Historiskt sett var 'cat', 'more' och 'less' de ursprungliga verktygen för att läsa en textfil i Unix, med 'cat' datandes tillbaka till Version 1 Unix.  

Som alternativ kan 'awk' och 'sed' också läsa en textfil, men dessa är oftast mer användbara för mer komplexa textbearbetningar. 

När du använder 'read' -kommandot med '-r' -flaggan, undviker du att tolka bakslag som escape-tecken. 'IFS=', eller det interna fältseparatorn, förhindrar att ledande och efterföljande mellanslag trimmas.

## Se också
['The Linux Documentation Project' - Text Processing Commands](http://tldp.org/LDP/GNU-Linux-Tools-Summary/html/x11655.htm)  
['GNU Bash Manual' - Bash Built-in Commands](https://www.gnu.org/software/bash/manual/bash.html#Bash-Builtins)
['Stackoverflow' - How to read a file line by line in Bash](https://stackoverflow.com/questions/10929453/read-a-file-line-by-line-assigning-the-value-to-a-variable)