---
title:                "Omvandla ett datum till en sträng"
html_title:           "Go: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Nu när vi är i det moderna programmeringsspråket Go (nuvarande version) kanske du undrar varför du skulle behöva konvertera ett datum till en sträng. Svaret är enkelt - för att utföra formatering, utskrift eller lagring av datum i ett specifikt format.

## Hur man gör
För att konvertera ett datum till en sträng i Go behöver vi använda funktionen `Format` från paketet `time`. Här är ett enkelt exempel:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	date := time.Now().Format("2006-01-02")
	fmt.Println(date)
}
```

I detta exempel använder vi `Now` för att få nuvarande datum och tid. Därefter använder vi `Format` för att formatera datumet enligt vårt önskade format, vilket i det här fallet är `2006-01-02`. Detta kan verka konstigt, men det är faktiskt standarden för Go-datum formatering eftersom det representerar år-månad-dag.
Output: `2020-01-22`

Detta var ett enkelt exempel, men vi kan använda mer avancerade formateringssträngar för att skräddarsy datumsträngen mer noggrant. Här är ett exempel för att visa datum och tid i en annan tidszon:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	loc, err := time.LoadLocation("Europe/Stockholm")
	if err != nil {
		fmt.Println("Felaktig tidszon: ", err)
	}
	date := time.Now().In(loc).Format("2006-01-02 15:04:05")
	fmt.Println(date)
}
```

Output: `2020-01-22 18:32:10`

## Djupdykning
Om du är ny i Go och undrar varför året i formateringssträngen är `2006` istället för `2020`, så är svaret enkelt. Go-användare identifierar och använder standardiserade tidsformat genom att använda den interna tidsvariabeln `time.RubyDate`, vilket i detta fall är år-månad-dag. Detta började som en intern skämt inom Go-samhället, men blev så populär att det nu är en standard.

Även om detta kanske inte verkar intuitivt, är det en bra standard att följa eftersom det hjälper till att undvika förvirring och förhindrar att datum skrivs i fel ordning.

## Se även
* [Go Language Tutorial - Date & Time](https://www.guru99.com/date-time-and-time-intervals-in-go-language.html)
* [Go Language Specification - Time](https://golang.org/ref/spec#Time)
* [Go by Example - Time Formatting/Parsing](https://gobyexample.com/time-formatting-parsing)