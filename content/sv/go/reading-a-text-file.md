---
title:    "Go: Läsa en textfil"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Varför

Att läsa och bearbeta textfiler är en grundläggande uppgift inom programmering som kan vara användbar i många olika situationer. Genom att läsa en textfil med Go kan du hitta specifika data, analysera mönster eller till och med bygga en helt ny applikation.

##Hur man gör det

För att läsa en textfil med Go kan du använda funktionen "os.Open" för att öppna filen och sedan skapa en "bufio.Scanner" för att läsa innehållet i filen. Här är ett exempel på hur du kan läsa en textfil rad för rad och skriva ut innehållet till konsolen:

```Go
file, err := os.Open("textfil.txt")
if err != nil {
  panic(err)
}
defer file.Close()

scanner := bufio.NewScanner(file)

for scanner.Scan() {
  fmt.Println(scanner.Text())
}

if err := scanner.Err(); err != nil {
  panic(err)
}
```

Ovanstående kod använder funktionen "os.Open" för att öppna filen "textfil.txt" och sedan en "bufio.Scanner" för att läsa rad för rad. För varje rad som läses skriver den ut innehållet till konsolen. Vi använder också "defer" för att stänga filen efter att vi är klara med den.

##Djupdykning

Förutom att läsa rad för rad kan vi också behandla innehållet i en textfil på andra sätt, som att separera strängar, hitta specifika ord eller tecken, och mycket mer. Genom att använda funktioner som "strings.Split" och "strings.Contains" kan vi bearbeta innehållet i filen på ett effektivt sätt.

En annan intressant funktion inom Go är "ioutil.ReadFile". Istället för att öppna och läsa filen manuellt, kan vi använda denna funktion för att läsa hela filinnehållet på en gång och sedan bearbeta det enligt våra behov.

##Se även

- [Go Dokumentation: Paketet "os"](https://golang.org/pkg/os/)
- [Go Dokumentation: Paketet "bufio"](https://golang.org/pkg/bufio/)
- [Go Dokumentation: Paketet "strings"](https://golang.org/pkg/strings/)
- [Go Dokumentation: Paketet "ioutil"](https://golang.org/pkg/ioutil/)