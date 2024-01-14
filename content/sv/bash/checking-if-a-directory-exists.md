---
title:                "Bash: Kontrollera om en mapp finns tillgänglig"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Varför

Att kolla om en mapp existerar är en viktig del av Bash programmering, särskilt när man jobbar med filsystem och behöver göra kontroller och beslut baserat på olika mappar. Genom att kolla om en mapp finns, kan du också undvika fel och driftstopp i ditt skript.

##Hur man gör

För att kolla om en mapp existerar i Bash, kan du använda kommandot [ -d ]. Detta kommando kollar om en given sökväg är en befintlig mapp och returnerar antingen sant eller falskt. Här är ett exempel som testar om en mapp "Documents" existerar i din nuvarande arbetsmapp.

```Bash
if [ -d Documents ]; then
    echo "Mappen Documents existerar."
else
    echo "Mappen Documents existerar inte."
fi
```

I detta exempel använder vi en "if" sats för att utföra en viss handling beroende på resultatet av testet. Om mappen existerar, kommer utskriften att vara "Mappen Documents existerar.", om inte, kommer utskriften att vara "Mappen Documents existerar inte." Observera att du måste använda utropstecken före kommandot [-d] för att dess output ska användas i en "if" sats.

Du kan också använda en variabel som sökväg för att testa om en mapp existerar. I det här exemplet, kollar vi om mappen som är lagrad i variabeln "folder" existerar.

```Bash
folder='/home/user/Pictures'
if [ -d $folder ]; then
  echo "$folder existerar."
else
  echo "$folder existerar inte."
fi
```

Om du vill kolla om en mapp INTE existerar, kan du använda [ ! -d ]. Till exempel, om du vill se till att en mapp "Videos" inte finns, kan du använda följande kod.

```Bash
if [ ! -d Videos ]; then
    echo "Mappen Videos finns inte."
fi
```

##Djupdykning

Kommandot [ -d ] kollar bara om en sökväg är en mapp eller inte. Det betyder att om sökvägen är en fil eller en symbolisk länk, kommer det att returnera falskt oavsett om en mapp med samma namn finns eller inte. För att undvika detta kan du använda [ -e ] - kommandot som kollar om en sökväg existerar oavsett om det är en mapp, fil, eller länk.

En annan användbar kommando för att kontrollera mappar är [ -w ] som kollar om mappen är skrivbar. Detta kan vara praktiskt när du behöver skapa eller ändra filer inuti en mapp men vill se till att du har tillstånd att göra det innan du fortsätter i ditt skript.

##Se även

- [Bash scripting tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Looping in Bash](https://www.geeksforgeeks.org/loops-in-bash-shell-scripting/)