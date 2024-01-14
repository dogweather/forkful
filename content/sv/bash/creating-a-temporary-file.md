---
title:    "Bash: Skapa en tillfällig fil"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför
Att skapa en temporär fil kan vara användbart i många olika situationer. Till exempel, om du behöver spara data för en kort stund i en fil, men inte behöver behålla filen permanent. Det är också användbart när du behöver skapa en fil för att skicka till någon annan eller använda i ditt program, men du vill inte skriva över en befintlig fil.

## Hur man gör det
För att skapa en temporär fil i Bash, kan du använda kommandot `mktemp`. Det finns två olika sätt att använda det på:

1. Om du bara behöver filnamnet, men inte filen ska skapas ännu, kan du använda `mktemp` med argumentet `-p` för att ange en målmapp och eventuella önskade prefix för filnamnet. Till exempel:

```
Bash mktemp -p ~/Documents/ utkastXXXX
```

Det här kommer att skapa ett filnamn som börjar med "utkast" och slutar med fyra slumpmässiga siffror i mappen "Documents" i din hemkatalog.

2. Om du vill skapa en tom fil direkt, kan du använda flaggan `-t` i kombination med `-p` och prefix. Till exempel:

```
Bash mktemp -p ~/Documents/ -t utkastXXXX > test.txt
```

Det här kommer att skapa en fil med namnet "utkastXXXX" i mappen "Documents" och omdirigera innehållet till test.txt. Du kan sedan använda filen "test.txt" som du skulle med vilken annan fil som helst.

## Djupdykning
När du skapar en temporär fil med `mktemp` skapas en unik fil med ett slumpmässigt genererat namn. Detta förhindrar att filen skrivs över av andra filer som redan finns i mappen. Dessutom tar `mktemp` hand om att ta bort filen när den inte längre används, så du behöver inte oroa dig för att rensa upp efter dig själv.

Du kan också ange andra argument till `mktemp` för att styra hur filen skapas och vad den heter. Till exempel, flaggan `-d` skapar en temporär mapp istället för en fil, `-u` tvingar det genererade namnet att vara unikt och `-q` tystar alla eventuella felmeddelanden.

## Se även
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [The Linux Command Line](http://linuxcommand.org/tlcl.php)