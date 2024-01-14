---
title:                "Bash: Skapa en temporär fil"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa temporära filer är en vanlig praxis inom Bash-programmering. Det är ett sätt att temporärt lagra data eller resultat under körning av skript. Det är även ett sätt att hålla din kod organiserad och undvika att göra permanenta ändringar i dina filer.

## Hur man gör
För att skapa en temporär fil i Bash, används "mktemp" kommandot. Detta kommando kommer att skapa en unik temporär fil med ett namn som börjar med "tmp." Följande kodblock visar en enkel användning av "mktemp" i Bash:

```Bash
temp_file=$(mktemp)
echo "Detta är en temporär fil" > $temp_file
cat $temp_file
```

När du kör detta kommer en temporär fil att skapas och utskriften "Detta är en temporär fil" kommer att läggas till i filen. Slutligen kommer innehållet av filen att skrivas ut och filen kommer att raderas automatiskt eftersom ingen permanent plats har angivits.

## Djupdykning
När du använder "mktemp" kommandot, kan du även välja att skapa en temporär mapp istället för en fil. Detta är användbart när du behöver lagra flera filer eller skapa en tillfällig arbetsplats. För att göra detta, lägger du bara till "-d" flaggan i "mktemp" kommandot:

```Bash
temp_dir=$(mktemp -d)
touch $temp_dir/file1.txt
touch $temp_dir/file2.txt
ls $temp_dir
```

I detta exempel skapas en temporär mapp och två filer läggs till i denna mapp. Denna metod kan användas för att skapa en temporär arbetsplats för att utföra komplexa uppgifter eller för att organisera tillfälliga filer.

## Se även
- [The Linux Command Line: A Complete Introduction](https://www.linuxcommand.org/tlcl.php)
- [Bash Scripting Tutorial for Beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [mktemp man-sida](https://linux.die.net/man/1/mktemp)