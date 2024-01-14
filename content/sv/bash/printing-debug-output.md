---
title:    "Bash: Utskrift av felsökningsspårning"
keywords: ["Bash"]
---

{{< edit_this_page >}}

##Varför##

Att skriva kod kan vara en komplicerad process och ibland kan man behöva felsöka för att hitta och åtgärda problem. Ett sätt att göra detta är genom att printa ut debug output, vilket kan ge viktig information för att förstå vad som händer i koden. I den här artikeln kommer vi att gå igenom hur du kan använda Bash för att printa debug output och varför det är en användbar teknik.

##Så här gör du## 

För att printa debug output i Bash, kan du använda kommandot "echo" som skriver ut text på skärmen. Detta är användbart när du vill se värdet på en variabel eller kontrollera om en viss del av koden körs. Se nedan för ett exempel: 

```Bash
#!/bin/bash 
# En enkel kod som ökar värdet på variabeln "summa" med 1
summa=1
(( summa++ ))
echo "Nya summan är:" $summa
```

I detta exempel, kommer "echo" kommandot att skriva ut "Nya summan är: 2" på skärmen, vilket hjälper oss att bekräfta att variabeln "summa" har ökats med 1. Du kan använda detta för att printa ut olika variabler, anropa funktioner och hitta felaktiga delar av koden.

##Gräv djupare## 

Förutom att använda "echo" för att printa ut debug output, kan du också lägga till flags för att göra det mer läsbart. En vanlig flagga som används för att printa debug output är "-e", vilket tolkar escape-tecken och gör det möjligt att formatera texten. Du kan också använda "-n" flaggan som förhindrar att en ny rad läggs till efter texten, vilket kan vara användbart när du vill printa flera rader av debug output utan att det blir rörigt. Se nedan för ett exempel:

```Bash
#!/bin/bash 
# En kod som skriver ut en lista över frukter genom att loopa igenom en array
frukter=("äpple" "banan" "kiwi" "apelsin")
echo -e "Här är en lista över frukter:\n"
for frukt in "${frukter[@]}"
do
  echo -n "- $frukt\n"
done
```

I detta exempel, kommer "-e" flaggan att tolka new line escape-tecknet "\n" och skapa en ny rad efter texten "Här är en lista över frukter:". "-n" flaggan förhindrar också att en ny rad läggs till i for-loopen, vilket resulterar i en snyggt utformad lista över frukter på en enda rad.

##Se även##

* [Officiell Bash-dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
* [Enkel guide till felsökning i Bash](https://linuxhint.com/bash_debug_mode)
* [10 användbara Bash-kommandon](https://www.tecmint.com/useful-linux-commands-for-system-administrators/)

Felsökning är en viktig del av processen att skriva kod och printa debug output är ett verktyg som kan hjälpa dig att hitta och åtgärda problem snabbt och effektivt. Med hjälp av dessa kommandon och flags som vi gått igenom i den här artikeln, kan du enkelt lägga till debug output i din kod och förbättra din felsökningsprocess. Glöm inte att kolla in de länkar vi har delat för mer information om felsökning i Bash. Lycka till!