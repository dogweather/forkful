---
title:    "Bash: Kontrollera om en mapp finns"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar kan vara en viktig del av Bash-programmering. Det kan hjälpa till att undvika felmeddelanden och säkerställa att programmets fortsätter att fungera smidigt utan avbrott. Om du är ny inom Bash-programmering kan det kännas överväldigande att lära sig hur man kontrollerar om en mapp existerar, men det är faktiskt inte så svårt som det låter.

## Hur man gör det

För att kontrollera om en mapp existerar kan du använda kommandot "test" i kombination med flaggan "-d". Testkommandot kontrollerar om en viss fil eller mapp existerar och återgår med ett utgångsvärde som antingen är "sant" eller "falskt". Flaggan "-d" talar om för testkommandot att endast kontrollera om det är en mapp och inte någon annan typ av fil. Här är ett exempel på hur du kan göra det:

```Bash
if test -d /path/to/directory
then
    echo "Mappen finns"
else
    echo "Mappen finns inte"
fi
```

Som du kan se kontrollerar vi om mappen med sökvägen "/path/to/directory" existerar och beroende på resultatet skriver ut antingen "Mappen finns" eller "Mappen finns inte". Om du vill kontrollera om mappen finns relativt till din nuvarande arbetskatalog kan du använda "." som sökväg och om du vill kontrollera om en mapp i din hemkatalog kan du använda "~". Här är ett exempel på hur du kan göra det:

```Bash
if test -d ~/Documents
then
    echo "Mappen finns"
else
    echo "Mappen finns inte"
fi
```

Det finns också en annan version av testkommandot som du kanske har sett i olika bash-skript, nämligen det dubbla hakparenteskommandot "[[ ]]" som också kan användas för att kontrollera om en mapp existerar. Här är ett exempel på hur du kan göra det:

```Bash
if [[ -d /path/to/directory ]]
then
    echo "Mappen finns"
else
    echo "Mappen finns inte"
fi
```

Skillnaden mellan dessa två kommandon är att det dubbla hakparenteskommandot ger mer flexibilitet och fler alternativ att kontrollera för olika villkor.

## Djupgående

Om du vill gå ännu djupare kan du också använda kommandot "ls" tillsammans med "grep" för att kontrollera direkt i en lista över mappar. Här är ett exempel:

```Bash
if ls | grep -q "mappnamn"
then
    echo "Mappen finns"
else
    echo "Mappen finns inte"
fi
```

I detta exempel använder vi kommandot "ls" för att lista alla filer och mappar i den aktuella arbetskatalogen och sedan använder vi "grep" för att leta efter en viss mappnamn. Om "grep" hittar mappnamnet i listan kommer det att returnera "sant" och skriva ut "Mappen finns".

## Se även

Här är några användbara länkar för vidare läsning om Bash-programmering:

- [Bash Documentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Beginner's Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)
- [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial)