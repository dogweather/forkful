---
title:                "Bash: Sammanslagning av strängar"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar är en vanlig uppgift inom programmering som används för att slå ihop flera mindre strängar till en större sträng. Detta är särskilt användbart när man vill skapa dynamiska texter eller meddelanden baserat på variabler eller användarinput.

## Hur man gör det

För att sammanfoga strängar i Bash används operatorn `+` eller genom att använda variabler inom dubbelfnuttar `" "` för att ersätta dem med deras faktiska värden.

```Bash
förnamn="Lisa"
efternamn="Svensson"

echo "Hej $förnamn $efternamn, välkommen till min blogg!"
```

Output:
```
Hej Lisa Svensson, välkommen till min blogg!
```

I detta exempel ersätts variablerna `$förnamn` och `$efternamn` med sina faktiska värden när de används inom dubbelfnuttar.

## Djupdykning

När man sammanfogar strängar i Bash är det viktigt att tänka på hur variabler hanteras. Om man använder en variabel utanför dubbelfnuttar kommer den inte att ersättas med sitt faktiska värde.

```Bash
namn="Johan"
echo 'Hej $namn'
```

Output:
```
Hej $namn
```

Detta beror på att variabeln inte tolkas inuti enkelfnuttar och därför kommer den inte att ersättas med sitt värde. För att undvika detta kan man antingen använda variabler inom dubbelfnuttar eller använda operatorn `+` för att sammanfoga strängar.

Det är också viktigt att notera att man inte kan sammanfoga strängar genom att använda operatorn `+` om någon av strängarna innehåller specialtecken som mellanslag eller tabbar. I sådana fall måste man använda sig av dubbelfnuttar och variabler för att ersätta dem med rätt värden.

## Se även

- Bash manual: [Concatenating Strings](https://www.gnu.org/software/bash/manual/html_node/Shell-Arithmetic.html#Shell-Arithmetic)
- Stack Overflow: [Concatenating Strings in Bash](https://stackoverflow.com/questions/1251999/how-can-i-concatenate-string-variables-in-bash)