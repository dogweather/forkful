---
title:                "Utskrift av debuggutdata"
html_title:           "Bash: Utskrift av debuggutdata"
simple_title:         "Utskrift av debuggutdata"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utdata är en viktig del av bash-programmering eftersom det ger utvecklare en snabb och effektiv metod att förstå hur deras kod fungerar. Detta är särskilt användbart när man letar efter fel och buggar i sin kod. 

## Så här gör du

Det är enkelt att skriva ut debug-utdata i bash-programmering. Allt du behöver göra är att använda kommandot "echo" och skriva ut den önskade informationen. Detta kan göras på flera sätt, nedan finns några exempel:

```Bash
# Skriv ut en enkel sträng
echo "Hello World"

# Skriv ut värdet på en variabel
name="John"
echo "My name is $name"

# Kombinera flera variabler och strängar
age=25
echo "Jag är $name och jag är $age år gammal"
```

Detta skulle generera följande utdata:

```
Hello World
My name is John
Jag är John och jag är 25 år gammal
```

## Djupdykning

Det finns flera sätt att anpassa och förbättra utskriften av debug-utdata i bash. Ett sätt är att använda flaggan "-e" med echo-kommandot för att tillåta användning av escape-tecken, som tillåter dig att formatera din utdata på ett önskat sätt. Till exempel:

```Bash
# Använd avstånd mellan olika delar av utdatan
echo -e "Namn: $name \nÅlder: $age \nLand: Sweden"

# Lägg till färg 
echo -e "Namn: $name \nÅlder: $age \nLand: \e[34mSweden\e[0m" 
# \e[34m representerar det blå färgen, medan \e[0m återställer färgen till standard

# Lägg till en horisontell linje
echo -e "Namn: $name \nÅlder: $age \nLand: United States \n---"
```

Detta skulle ge ut följande utdata:

```
Namn: John 
Ålder: 25 
Land: Sweden
```

```
Namn: John 
Ålder: 25 
Land: Sweden
```

```
Namn: John 
Ålder: 25 
Land: Sweden
```

Att skriva ut debug-utdata i bash ger dig också möjlighet att spara utdatan till en fil istället för att bara visa den i terminalen. För att göra detta kan du enkelt använda redirection-operatorn ">>". Till exempel:

```Bash
# Spara utdatan till en fil
echo "Hello World" >> output.txt

# Kombinera flera variabler och spara till en fil
echo -e "Namn: $name \nÅlder: $age \nLand: Sweden" >> output.txt
```

Detta skulle skapa en fil med namnet "output.txt" och spara följande utdata i filen:

```
Hello World
```

```
Namn: John 
Ålder: 25 
Land: Sweden
```

## Se även

Här är några länkar som kan hjälpa dig att lära dig mer om att skriva ut debug-utdata i bash-programmering:

- [Echo Command in Bash](https://linuxize.com/post/echo-command-in-bash/)
- [How to use colored output in bash script](https://stackoverflow.com/questions/16843382/how-to-use-colored-output-in-bash-script)
- [Bashing Bash: How to redirect output to a file](https://www.networkworld.com/article/2696144/bashing-bash-how-to-redirect-output-to-a-file.html)