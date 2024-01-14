---
title:                "Bash: Sammanfogning av strängar"
simple_title:         "Sammanfogning av strängar"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

I Bash-programmering är det vanligt att man behöver kombinera (konkatinera) flera strängar för att skapa en komplett text. Detta kan vara användbart när man till exempel behöver skapa en dynamisk filnamn eller en användarvänlig utskrift.

## Hur man gör

För att sätta samman två eller flera strängar i Bash använder man sig av operatorn + (plus). Här är ett exempel på hur man kan göra det:

```Bash
förnamn="Maria"
efternamn="Johansson"

echo "$förnamn $efternamn" # Output: Maria Johansson
```

Du kan även kombinera strängar med variabler och andra tecken. Här är ett exempel med specialtecknet "!" och en siffra:

```Bash
förnamn="Maria"
nummer=42

echo "$förnamn! $nummer" # Output: Maria! 42
```

## Utforska djupare

Om du vill gå djupare in i konkatenering av strängar i Bash, kan du använda dig av kommandot `printf`. Detta kommando ger dig mer flexibilitet när det gäller formatering och utforskning av variabler tillsammans med text.

Här är ett exempel som visar hur man kan använda `printf` tillsammans med en variabel och strängar:

```Bash
namn="Lisa"
printf "Hej, mitt namn är %s" $namn # Output: Hej, mitt namn är Lisa
```

## Se även

Här är några länkar som kan vara användbara för dig när du utforskar mer om konkatenering av strängar i Bash:

- [Bash Guide for Beginners: String Manipulations](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_10_04.html)
- [Bash Hackers Wiki: String Operations](http://wiki.bash-hackers.org/commands/classictest#string_operations)
- [Bash man-sidan för `printf`](https://linux.die.net/man/1/printf)