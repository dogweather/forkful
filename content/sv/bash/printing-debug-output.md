---
title:                "Utmatning av felsökningsresultat"
html_title:           "Bash: Utmatning av felsökningsresultat"
simple_title:         "Utmatning av felsökningsresultat"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Debugutskrift är en teknik som används av programmerare för att identifiera och lösa fel i sin kod. Genom att skriva ut olika värden och meddelanden under exekveringen av koden kan man få en bättre förståelse för vad som händer och varför. Detta underlättar felsökningen och resulterar i en mer effektiv kod.

# Så här gör du:

Det finns flera sätt att skriva ut debug output i Bash. Ett vanligt sätt är att använda kommandot ```echo``` och skriva ut variabler eller textsträngar. Till exempel, om vi har en variabel ```name="John"``` kan vi skriva ut värdet med kommandot ```echo $name``` vilket ger output "John". Det går också att kombinera flera variabler eller textsträngar, t.ex. ```echo "Hello, my name is $name"``` som ger output "Hello, my name is John". 

Ett annat sätt är att använda kommandot ```printf``` för att formatera och skriva ut meddelanden. Syntaxen liknar vanlig printf i C-programmeringsspråket, t.ex. ```printf "My name is %s, and I'm %d years old" $name $age``` vilket ger output "My name is John, and I'm 30 years old". Det finns även andra användbara alternativ som ```-v``` för att spara outputen i en variabel och ```-e``` för att tolka escape-tecken som \n (newline) eller \t (tab).

# Djupdyka:

Att skriva ut debug output är en vanlig och betydelsefull del av felsökning inom programutveckling. Det har funnits sedan de tidiga Unix-tiderna och är en naturlig del av Bash-skript som inte har en interaktiv debugger som andra programmeringsspråk. Det finns också andra alternativ som ```set -x``` för att få detaljerad information om exekveringsstegen, eller ```trap``` för att fånga och hantera fel i koden.

Ett annat användningsområde för debugutskrift är att optimera prestanda och hitta flaskhalsar i koden. Genom att mäta och jämföra utskriftstiden för olika delar av koden kan man identifiera vilka som tar mest tid och behöver optimeras.

Implementationen av debugutskrift är relativt enkel och kan med fördel integreras i utvecklingsprocessen från början. Det är viktigt att hålla koll på vilken typ av information som skrivs ut och att ta bort onödig eller känslig data innan koden går live.

# Se även:

Det finns många guider och artiklar på internet om hur man skriver ut debug output i Bash. Här är några användbara resurser: 
- [Bash Debugging Guide](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_02_03.html)
- [Debugging Bash Scripts](https://www.shellscript.sh/debugging.html) 
- [Bash Debugging Techniques](https://www.shell-tips.com/bash/debugging-bash-scripts/)