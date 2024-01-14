---
title:                "Go: Sammanfogning av strängar"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Att sammanfoga eller sammanslå strängar är en vanlig operation som används i många programmeringsspråk, inklusive Go. Detta gör det möjligt att skapa dynamiska och anpassningsbara strängar som kan användas för att manipulera och visa data på ett effektivt sätt.

## Hur man gör
För att sammanfoga strängar i Go behövs endast ett par enkla steg. Först måste du definiera de strängar som du vill sammanfoga och tilldela dem till variabler. Sedan kan du använda "+" operatorn för att kombinera strängarna och skapa en ny sträng. Se nedan för ett exempel:

```Go
// Definiera variabler
förnamn := "Anna"
efternamn := "Andersson"

// Slå ihop strängarna
fulltNamn := förnamn + " " + efternamn

// Skriv ut resultatet
fmt.Println(fulltNamn)
```
Output: Anna Andersson

Du kan också använda en funktion som heter "Sprintf" för att sammanfoga strängar. Detta kan vara användbart om du vill ange ett specifikt format för resultatet. Exempelvis:

```Go
// Definiera variabler
ålder := 25
höjd := 170.2

// Slå ihop strängar med hjälp av Sprintf
info := fmt.Sprintf("Ålder: %d, Längd: %.1f cm", ålder, höjd)

// Skriv ut resultatet
fmt.Println(info)
```
Output: Ålder: 25, Längd: 170.2 cm

Det är också möjligt att sammanfoga flera strängar samtidigt genom att använda en variabel av typen "[]string" och sedan använda "Join" funktionen. Exempelvis:

```Go
// Definiera en variabel av typen []string
favoritFrukter := []string{"Äpple", "Banan", "Apelsin"}

// Slå ihop strängar med hjälp av Join
strängar := strings.Join(favoritFrukter, ", ")

// Skriv ut resultatet
fmt.Println(strängar)
```
Output: Äpple, Banan, Apelsin

## Djupdykning
I Go, som i många andra språk, är strängar inte föränderliga (immutable), vilket betyder att de inte kan ändras när de väl är skapade. Så när du "slår ihop" strängar, skapar du faktiskt en helt ny sträng som är en kombination av de ursprungliga strängarna.

Något annat att vara medveten om är att sammanfogning av strängar kan påverka prestandan om det görs i en loop eller i stora mängder. Detta beror på att en ny sträng måste skapas varje gång operationen utförs, vilket kan leda till onödigt minnesanvändning.

## Se också
- Officiell Go Dokumentation: https://golang.org/doc/effective_go.html#strings 
- Sammanfoga strängar med Sprintf: https://golang.org/pkg/fmt/#Sprintf 
- Sammanfoga strängar med Join: https://golang.org/pkg/strings/#Join