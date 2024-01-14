---
title:                "Bash: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att skriva en textfil är ett enkelt och effektivt sätt att organisera och lagra information. Det är också användbart för att skapa och köra skript och program i Bash.

## Så här gör du
För att skapa en textfil i Bash, öppna terminalen och navigera till den mapp där du vill skapa filen. Sedan kan du använda kommandot "touch" följt av namnet på filen för att skapa den. Till exempel, om du vill skapa en fil som heter "mitt_exempel.txt", skulle kommandot vara "touch mitt_exempel.txt".
```Bash 
touch mitt_exempel.txt
```
Nästa steg är att öppna filen för redigering. Detta kan göras med ett textredigeringsprogram som "nano" eller "vim". Till exempel, om du vill öppna filen "mitt_exempel.txt" med nano, skulle kommandot vara "nano mitt_exempel.txt". 
```Bash 
nano mitt_exempel.txt
```

När filen är öppen kan du börja skriva din text. För att spara och stänga filen i nano, tryck på "ctrl + x" och sedan "y" för att bekräfta. Om du använder vim, tryck på "esc" för att gå ut i normalt läge, skriv ":wq" och tryck på enter för att spara och stänga filen.

## Djupdykning
En textfil består av vanligtvis av text som kan läsas och redigeras av en dator eller textredigeringsprogram. De kan också innehålla olika typer av data, såsom siffror, symboler och specialtecken.

För att lägga till fler rader i en textfil kan du använda kommandot "echo" följt av texten du vill lägga till och sedan använda redirection operatorn ">>" för att skicka in det till filen. Till exempel, om du vill lägga till "Det här är en andra rad" i filen "mitt_exempel.txt", skulle kommandot vara:
```Bash 
echo "Det här är en andra rad" >> mitt_exempel.txt
```
För att läsa innehållet i en textfil, använd kommandot "cat" följt av namnet på filen. Till exempel, för att läsa innehållet i "mitt_exempel.txt", skulle kommandot vara:
```Bash 
cat mitt_exempel.txt
```

## Se även
- [BashGuide](http://guide.bash.se/)
- [Lär dig Bash i 15 minuter](https://www.digitalocean.com/community/tutorials/how-to-read-and-set-environmental-and-shell-variables-on-a-linux-vps)