---
title:    "Bash: Sammanfogning av strängar"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför
Att konkatenera strängar är ett viktigt verktyg inom Bash-programmering eftersom det låter dig kombinera flera strängar till en enda, vilket kan vara användbart i många olika scenarion.

## Så här gör du
För att konkatenera strängar kan du använda dig av operatorn `+` eller genom att helt enkelt skriva ihop strängarna med en mellanslag mellan. Här är ett enkelt exempel:

```Bash
string1="Hej"
string2="världen!"
echo $string1$string2
```

Detta kommer att ge följande utmatning:

```
Hej världen!
```

Det går även bra att konkatenera variabler, som i följande exempel:

```Bash
name="Emma"
language="Bash"
echo "Hej $name, välkommen till $language-gemenskapen!"
```

Vilket kommer att ge utmatningen:

```
Hej Emma, välkommen till Bash-gemenskapen!
```

## Djupdykning
Det finns några viktiga saker att tänka på när du konkatenerar strängar i Bash. En är att du behöver vara noga med mellanslag och andra specialtecken, annars kan det leda till oönskade effekter. Du kan också använda dig av en `+=` operator om du vill konkatenera flera strängar till en variabel. Det finns även andra och mer avancerade tekniker för att konkatenera strängar i Bash, som du kan läsa mer om i Bash-dokumentationen.

## Se även
- [Bash dokumentation](https://www.gnu.org/software/bash/) 
- [Bash-konkatenering för nybörjare](https://medium.com/@codingnikola/bash-concatenation-for-beginners-137b0bd1bc61) 
- [Bash strängmanipulering](https://ryanstutorials.net/bash-scripting-tutorial/bash-strings.php)