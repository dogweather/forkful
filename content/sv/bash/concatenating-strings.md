---
title:                "Sammanslagning av strängar"
html_title:           "Bash: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar är en vanlig uppgift inom programmering, särskilt inom Bash-scripting. Genom att kombinera flera strängar till en kan du skapa mer dynamiska och intuitiva skript.

## Hur man gör

För att sammanfoga strängar i Bash använder vi operatorn ```+=``` tillsammans med variabler. Vi tilldelar den nya strängen till variabeln genom att använda ```$``` innan variabelnamnet. Här är ett exempel på hur detta kan se ut:

```Bash
name="Lisa"
greeting="Välkommen till vår hemsida, "
greeting+=name
echo $greeting
```

Output: Välkommen till vår hemsida, Lisa

Som du kan se har vi sammanfogat variabeln "name" med strängen "Välkommen till vår hemsida, " genom att använda ```+=``` operatorn. Detta skapar en ny sträng som vi sedan skriver ut med hjälp av ```echo``` kommandot.

## Djupdykning

I Bash är det möjligt att sammanfoga flera strängar på en gång genom att använda operatorn ```${var1}${var2}```. Här är ett exempel på hur detta kan se ut:

```Bash
firstName="Lisa"
lastName="Johansson"
fullName=${firstName}${lastName}
echo $fullName
```

Output: LisaJohansson

Det är också möjligt att använda strängformatering för att skapa mer strukturerade utskrifter. Ett vanligt sätt att göra detta på är genom att använda ```printf``` kommandot. Här är ett exempel på hur detta kan se ut:

```Bash
name="Lisa"
age=25
printf "Hej, mitt namn är %s och jag är %d år gammal.\n" $name $age
```

Output: Hej, mitt namn är Lisa och jag är 25 år gammal.

Det finns även andra sätt att sammanfoga strängar i Bash, såsom användning av ```$(command)``` för att köra kommandon och kombinera deras utdata med strängar.

## Se även

Här är några användbara länkar för att lära dig mer om att sammanfoga strängar i Bash:

- https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion
- https://linuxize.com/post/bash-concatenate-strings/
- https://www.shellscript.sh/variables1.html