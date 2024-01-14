---
title:    "Bash: Sammanslagning av strängar"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar är en vanlig uppgift inom Bash-programmering. Genom att kombinera flera strängar kan vi skapa mer dynamiska och anpassade meddelanden och utdata i våra skript. Det är också ett sätt att hålla koden renare och mer lättläslig.

## Hur man gör

För att sammanfoga strängar i Bash använder vi oss av operatorn "+", vilket indikerar att två strängar ska kombineras. Vi kan också använda oss av metoden "printf" för att formatera och sammanfoga flera strängar.

```Bash
strang1="Hej"
strang2="världen!"
echo $strang1$strang2

# Resultat: Hej världen!
```

Vi kan också använda oss av variabler för att sammanfoga flera strängar på ett mer dynamiskt sätt.

```Bash
namn="Johan"
halsning="Hej"
echo $halsning $namn", välkommen till min blogg!"

# Resultat: Hej Johan, välkommen till min blogg!
```

En annan metod är att använda sig av "printf" för att ange specifika format för våra strängar.

```Bash
namn="Maria"
echo "Välkommen %s, till vår hemsida!" | printf $namn

# Resultat: Välkommen Maria, till vår hemsida!
```

## Djupdykning

När vi sammanfogar strängar i Bash är det viktigt att vara medveten om korrekt formatering. Till exempel, om vi vill ange citattecken i en sträng så måste vi använda en escape-sekvens för att Bash inte ska tolka dem som en del av strängen.

```Bash
citat="\"En vän är en som vet allt om dig och ändå tycker om dig.\""
echo $citat

# Resultat: "En vän är en som vet allt om dig och ändå tycker om dig."
```

Vi kan också använda oss av funktionen "bc" för att sammanfoga strängar med matematiska operationer.

```Bash
tal1=5
tal2=8
echo "Resultatet är: "$tal1"+"$tal2"="$(($tal1 + $tal2))| bc

# Resultat: Resultatet är: 5+8=13
```

## Se också

- [Bash Beginners Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash scripting tutorial for beginners](https://www.shellscript.sh/tutorial.html)
- [The Linux Documentation Project](https://tldp.org/)