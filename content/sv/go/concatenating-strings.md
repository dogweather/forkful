---
title:                "Go: Sammansättning av textsträngar"
simple_title:         "Sammansättning av textsträngar"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga eller "concatenate" strängar är en vanlig uppgift inom programmering, och är särskilt användbart när vi vill skapa en längre text eller ett meddelande genom att kombinera flera olika delar av information. I Go-språket finns det flera sätt att utföra denna uppgift, vilket gör det till en fördelaktig kunskap för alla Go-programmerare.

## Hur man gör det

Först och främst måste vi förstå att en sträng i Go är en samling av tecken eller bytes. Vi kan skapa en sträng genom att använda enkla citattecken runt vår text, till exempel "Hej" eller 'Det här är en sträng'. För att sammanfoga två strängar tillsammans används '+' operatorn, som i följande exempel:

```Go
fname := "John"
lname := "Smith"
fullname := fname + " " + lname
fmt.Println(fullname)
```
Output: John Smith

Som du kan se i exemplet ovan, har vi definierat två variabler - "fname" och "lname" - som innehåller de två delarna av namnet. Genom att använda '+' operatörn har vi sedan skapat en ny variabel "fullname" som är en kombination av de två strängarna och utskrivit den. Genom att lägga till ett mellanslag mellan de två variablerna skapar vi en mellanrum eller ett "whitespace" mellan de två orden när de sammanfogas.

Det finns också andra sätt att sammanfoga strängar i Go, som att använda metoden "Join" från "strings" paketet eller "Sprintf" från "fmt" paketet. Det är viktigt att notera att strängar i Go är oföränderliga, vilket betyder att när en sträng väl har deklarerats, kan den inte ändras. Därför skapas i själva verket en ny sträng varje gång vi sammanfogar två strängar.

## Djupdykning

Det finns flera andra aspekter att överväga när man sammanfogar strängar i Go. Till exempel, om vi sammanfogar flera strängar i en loop, såsom:

```Go
result := ""
for i := 1; i < 5; i++ {
  result += "num" + strconv.Itoa(i)
}
```
I det här exemplet, eftersom strängar är oföränderliga, skapas en ny sträng för varje iteration av loopen, vilket kan bli ineffektivt för större strängar eller långa loopar. För att hantera detta, kan vi använda metoden "Join" från "strings" paketet, vilket kan hjälpa till att förbättra prestandan och minimera användningen av minne och CPU-tid.

En annan aspekt att tänka på när man sammanfogar strängar är Unicode. Go hanterar Unicode på ett mycket effektivt sätt, men det kan orsaka problem när man sammanfogar strängar från olika källor, som till exempel från en databas eller från användarinput. Det är viktigt att se till att de olika strängarna är kodade på ett enhetligt sätt för att undvika problem med teckenkodning.

## Se också

* [Go Dokumentation om stränghantering](https://golang.org/pkg/strings/)
* [Effektiv Go: Strängmanipulering](https://golang.org/doc/effective_go.html#string_manipulation)
* [Golang – Strängar](https://riptutorial.com/go/topic/2861/strings)