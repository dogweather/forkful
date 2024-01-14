---
title:                "Bash: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Skapandet av temporära filer är ett vanligt förekommande problem som kan uppstå inom Bash-programmering. Det kan användas för att temporärt lagra data som endast behövs under en kort stund eller för att hantera flera processer som behöver åtkomst till samma fil. Att förstå hur man skapar och hanterar temporära filer är därför en viktig färdighet för alla som arbetar med Bash.

## Hur man gör

Skapandet av en temporär fil i Bash är en ganska enkel process. Det finns flera metoder att välja mellan, men en av de vanligaste är att använda kommandot `mktemp`. Denna kommando skapar en temporär fil på ett unikt genererat namn och möjliggör åtkomst till filen via en variabel.

En grundläggande kod för att skapa en temporär fil i Bash skulle se ut så här:

```Bash
temp=$(mktemp)
echo "Min temporära fil är $temp"
```

Den första raden använder `mktemp` för att skapa en temporär fil och tilldelar den till en variabel som heter `temp`. I den andra raden skriver vi ut sökvägen till den skapade filen. Om vi kör denna kod skulle vi få en utmatning som ser ut så här:

```
Min temporära fil är /tmp/tmp.aPHZyK
```

Som du kan se så har en unik sökväg för den temporära filen genererats av `mktemp` och tilldelats till variabeln `temp`.

## Djupdykning

I djupdykningsdelen ska vi titta närmare på kommandot `mktemp` och hur det fungerar för att skapa temporära filer.

För det första så finns det flera mönster som kan användas när man skapar en temporär fil med `mktemp`. Det vanligaste mönstret är `XXX` som ersätts med ett slumpmässigt numeriskt eller alfanumeriskt värde. Men det finns också andra mönster som kan användas för att skapa mer specifika filer.

Det finns också en rad olika flaggor som kan användas med `mktemp`. En av de vanligaste är `-d` som används för att skapa en temporär katalog istället för en fil.

Det är också värt att notera att `mktemp` har inbyggd säkerhet för att förhindra att flera processer skapar en fil med samma namn. Detta gör det till ett säkert sätt att skapa temporära filer inom Bash.

## Se även

- [`mktemp` man-sida](https://www.commandlinux.com/man-page/man1/mktemp.1.html)
- [Bash-hemsida](https://www.gnu.org/software/bash/)
- [Bash-guide för nybörjare](https://tldp.org/LDP/Bash-Beginners-Guide/html/)