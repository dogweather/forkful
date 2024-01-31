---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-19
html_title:           "Arduino: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är processen att verifiera om en viss katalog redan existerar i filsystemet. Programmerare gör detta för att undvika fel vid filoperationer och för att säkerställa att skriptet beter sig som förväntat.

## Hur man gör:
För att checka om en katalog finns, använd `-d` flaggan i `if` satser. Här är några exempel hur man kan göra det:

```Bash
if [ -d "$DIRECTORY" ]; then
    echo "Katalogen finns."
else
    echo "Katalogen finns inte."
fi
```

Eller med `[[ ]]` som är mer modern syntax:

```Bash
if [[ -d "$DIRECTORY" ]]; then
    echo "Katalogen finns."
else
    echo "Katalogen finns inte."
fi
```

Testa skriptet med olika kataloger för att se olika resultat:

```Bash
DIRECTORY=/någon/katalog

if [[ -d "$DIRECTORY" ]]; then
    echo "Katalogen finns."
else
    echo "Katalogen finns inte."
fi
```

Sample Output:

```
Katalogen finns inte.
```

## Fördjupning
Möjligheten att kontrollera filsystemet i Bash har funnits sedan tidiga versioner av shellskriptning, vilket reflekterar Unix-filosofin att allt är en fil – inklusive kataloger. Andra alternativ för att kontrollera existerande kataloger inkluderar att använda `test` kommandot eller kommandot `find`, men de kan vara överdrivet komplicerade för så enkla operationer.

`test -d` och `[[ -d ]]` är snabbare och enklare sätt att göra jobbet. De skiljer sig åt där `[[ ]]` är en mer modern version som erbjuder förbättrad funktionalitet, som att till exempel hantera strängar som innehåller blanksteg utan behov av att citera dem.

Under motorhuven använder Bash olika systemanrop för att kontrollera tillståndet av filsystemet. Flaggor som `-d` ber operativsystemet att bekräfta om sökvägen refererar till en vanlig katalog och inte en fil eller en annan resurs.

## Se även
- [GNU Bash manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/index.html)
- [Stack Overflow: "How to check if a directory exists in a Bash shell script?"](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script)
