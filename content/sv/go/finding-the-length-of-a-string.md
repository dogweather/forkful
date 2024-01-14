---
title:    "Go: Att hitta längden av en sträng"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande operation inom programmering. Genom att förstå hur detta görs kan du använda denna kunskap i olika situationer, till exempel när du behöver validera inmatad text eller manipulera data i en sträng.

## Hur man gör

För att hitta längden på en sträng i Go behöver du först importera standardpaketet "strings". Sedan kan du använda funktionen "len()" för att bestämma längden på en sträng. Här är ett enkelt exempel:

```Go
import "strings"

func main() {
    str := "Det här är en sträng"
    length := len(str)
    fmt.Println(length) // output: 20
}
```

I det här exemplet använder vi funktionen "len()" för att bestämma längden på strängen "str". Vi skriver sedan ut resultatet med hjälp av "fmt.Println()"-funktionen. Resultatet visar att strängen består av 20 tecken.

Det finns också andra sätt att hitta längden på en sträng, till exempel genom att använda funktionen "strings.Count()". Denna funktion räknar antalet förekomster av en viss delsträng i en större sträng. Om du anger en tom delsträng kommer funktionen att räkna antalet tecken i den stora strängen.

```Go
import "strings"

func main() {
    str := "Hej, jag heter Johan"
    length := strings.Count(str, "")
    fmt.Println(length) // output: 19
}
```

I detta exempel använder vi funktionen "strings.Count()" för att räkna antalet tecken i strängen "str". Genom att ange en tom delsträng räknas antalet tecken i den stora strängen istället för antalet förekomster av den tomma delsträngen.

## Djupdykning

Att hitta längden på en sträng kan verka enkelt, men det finns vissa saker som är bra att ha i åtanke. En sak att tänka på är att funktionen "len()" räknar antalet bytes, inte antalet tecken. Detta kan vara viktigt om du arbetar med icke-latinska tecken eller emoticons.

Att använda funktionen "strings.Count()" kan också leda till oväntade resultat om din sträng innehåller icke-printbara tecken som till exempel mellanslag eller remsor. Det är därför viktigt att alltid kontrollera vad som ska räknas för att få önskat resultat.

## Se även

- Dokumentation för Go-paketet "strings": https://golang.org/pkg/strings/
- Tutorial om Go-språket: https://tour.golang.org/
- Forum för Go-programmerare: https://forum.golangbridge.org/