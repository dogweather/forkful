---
title:                "Fish Shell: Att läsa en textfil"
simple_title:         "Att läsa en textfil"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa och använda en textfil är ett vanligt scenario när man programmerar. Det kan användas för att lagra data, spara inställningar eller hantera kommandoradsargument. Det är viktigt att veta hur man läser och manipulerar en textfil för att kunna effektivisera sitt programmeringsarbete.

## Så här gör man
För att läsa en textfil i Fish Shell, kan följande kod användas:

```Fish Shell
$ cat file.txt
```

Denna kod läser innehållet i filen "file.txt" och skriver ut det på skärmen. Om man vill spara innehållet i en variabel, kan man använda följande kod:

```Fish Shell
$ set content (cat file.txt)
```

Genom att nu skriva ut variabeln "content" kommer innehållet i filen att visas på skärmen. Om man vill läsa innehållet rad för rad, kan man använda en "while loop" i följande kod:

```Fish Shell
while read -r line
    echo $line
end < file.txt
```

Detta kommer att läsa innehållet i filen "file.txt" och skriva ut en rad i taget tills hela filen har lästs.

## Djupdykning
När man läser en textfil, läses innehållet rad för rad. Det finns också möjlighet att läsa en specifik rad eller ett visst antal rader från en fil. Detta kan göras med hjälp av kommandot "head" och "tail".

Om man vill läsa den första raden i en fil, kan man använda följande kod:

```Fish Shell
$ cat file.txt | head -n 1
```

Detta kommer att läsa den första raden i filen "file.txt" och skriva ut den på skärmen. Om man vill läsa de fem första raderna i filen, kan man använda:

```Fish Shell
$ cat file.txt | head -n 5
```

På samma sätt kan man använda kommandot "tail" för att läsa de sista raderna i filen.

## Se även
- [Fish Shell's officiella dokumentation för att läsa en textfil](https://fishshell.com/docs/current/cmds/cat.html)
- [En guide för att läsa en fil rad för rad i Fish Shell](https://www.cyberciti.biz/faq/bash-loop-over-file/)
- [Kommandot "head" i Fish Shell](https://fishshell.com/docs/current/cmds/head.html)
- [Kommandot "tail" i Fish Shell](https://fishshell.com/docs/current/cmds/tail.html)