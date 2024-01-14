---
title:    "Bash: Läsning av kommandoradsargument"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför 
Om du är ny till programmering eller bara vill lära dig ett nytt språk, finns det många olika programmeringsspråk att utforska. Men ibland kan det vara bra att börja med ett enkelt och användbart språk, som till exempel Bash. Ett av de viktigaste koncepten i Bash-programmering är hantering av kommandoradsargument. Genom att kunna läsa och använda kommandoradsargument kan du göra dina Bash-skript mer interaktiva och anpassningsbara. I den här bloggposten kommer vi att gå igenom varför det är viktigt att kunna läsa kommandoradsargument och hur du kan göra det.

## Så här gör du
För att kunna läsa kommandoradsargument i Bash, måste vi först förstå hur de fungerar. När du kör ett Bash-skript från kommandoraden, kan du skicka med kommandoradsargument som parametrar till skriptet. Dessa argument kan sedan användas inuti skriptet för att manipulera eller visa olika resultat. 

En enkel kodexempel kan se ut så här:
```Bash
#!/bin/bash

echo "Hej $1! Välkommen till min blogg!" 
```
I det här exemplet kommer vi att hälsa användaren välkommen genom att använda det första kommandoradsargumentet som skickas in i skriptet. Om vi till exempel skriver ```bash script.sh Alice``` i kommandoraden, kommer skriptet att skriva ut "Hej Alice! Välkommen till min blogg!". 

Om du vill ta emot flera kommandoradsargument, kan du använda variabeln $2, $3, osv. beroende på hur många argument du vill ta emot. Du kan också använda variabeln $@ för att få en lista över alla kommandoradsargument som skickats in. 

## Utforska Djupare 
Det är också möjligt att läsa kommandoradsargument i en översättningsprocessning, där skriptet tar in flera flaggor och värden från kommandoraden. Detta kan vara speciellt användbart för skript som kräver olika inställningar beroende på vad användaren vill uppnå. En avancerad kodexempel kan se ut så här:
```Bash
#!/bin/bash

while getopts ":f:n:r:" option; do
  case ${option} in
    f ) file=$OPTARG;;
    n ) name=$OPTARG;;
    r ) remove=$OPTARG;;
    \? ) echo "Felaktig flagga: $OPTARG" 1>&2; exit 1;;
    : ) echo "Flaggan $OPTARG kräver ett argument." >&2; exit 1;;
  esac
done

echo "$name, välkommen till vår blogg!"
echo "Vi kommer att arbeta med filen $file."

if [ $remove -eq 1 ]; then
  echo "Filen $file kommer att rensas."
  # logik för att rensa filen
else 
  echo "Inga ändringar kommer att göras till filen."
fi
```

Här använder vi getopts för att ta emot flaggor och värden från kommandoraden och sedan utnyttja dessa för att göra olika åtgärder. I det här exemplet kan användaren välja att ange ett filnamn (-f), ett namn för hälsningsmeddelandet (-n) och en flagga för att rensa filen (-r). 

## Se även
- [En kort guide till Bash programmering](https://www.tecmint.com/beginners-guide-to-writing-bash-scripts/)
- [En introduktion till kommandoradsargument i Bash](https://opensource.com/article/19/11/bash-command-line-arguments)
- [Officiell dokumentation för Bash](https://www.gnu.org/software/bash/manual/)