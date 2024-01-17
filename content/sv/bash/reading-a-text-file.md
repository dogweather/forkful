---
title:                "Läsa en textfil"
html_title:           "Bash: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil är ett vanligt sätt för programmerare att läsa och manipulera data som är lagrat i en textbaserad form. Det kan vara allt från rådata från en sensor till text i en fil som behöver bearbetas.

## Så här gör du:
Att läsa en textfil i Bash är enkelt. Du kan använda kommandot `cat` för att skriva ut innehållet i filen direkt i terminalen. Till exempel om du har en fil som heter `minfil.txt` så kan du skriva `cat minfil.txt` för att se allt innehåll i filen.

Om du vill spara innehållet i filen till en variabel så kan du använda kommandot `read`. Till exempel kan du skriva `read filinnehall < minfil.txt` för att lagra innehållet i `minfil.txt` i variabeln `filinnehall`.

Du kan också använda for-loopar för att läsa filen rad för rad. Till exempel:

```
for rad in $(cat minfil.txt)
do
  echo $rad
done
```

Detta kommer att skriva ut varje rad i filen `minfil.txt` i terminalen.

## Djupdykning:
Att läsa en textfil är ett fundamentalt koncept inom programmering och är ett sätt att få tillgång till och bearbeta data i en enkel och läsbar form. Det finns dock alternativ till att använda `cat`-kommandot, såsom `sed` och `awk` som är mer kraftfulla och kan användas för mer avancerad manipulation av filer.

En annan viktig aspekt att tänka på är kodningsstandarder för textfiler, såsom ASCII och UTF-8. Det är viktigt att vara medveten om vilken standard som används för att undvika problem med läsningen av filen.

## Se även:
- [Bash dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Sed tutorial](https://www.grymoire.com/Unix/Sed.html)
- [AWK tutorial](https://www.tutorialspoint.com/awk/index.htm)