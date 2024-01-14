---
title:    "Go: Skapa en tillfällig fil"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

När du utvecklar program i Go, kanske du ibland behöver skapa temporära filer. Dessa filer kan användas för att tillfälligt lagra data eller hantera temporära processer. I denna bloggpost kommer vi att utforska varför och hur du kan skapa temporära filer i Go.

## Hur Man Gör

För att skapa en temporär fil i Go behöver vi importera paketet "os". Sedan kan vi använda funktionen TempFile() för att skapa en fil och få åtkomst till dess namn.

```Go
import "os"

tempFile, err := os.TempFile("", "temp")
if err != nil {
	fmt.Println("Error creating temporary file:", err)
	return
}
defer tempFile.Close()

fmt.Println("Temporary file name:", tempFile.Name())
```

I det här exemplet använder vi en tom sträng som första argument eftersom vi inte behöver en specifik mapp för vår temporära fil. Det andra argumentet "temp" är prefixet som läggs till filnamnet, vilket kan vara användbart för att identifiera filen senare.

När vi har skapat filen måste vi se till att stänga den igen när vi är klara med den, därför använder vi "defer" för att funktionen Close() ska köras efter att resten av koden har exekverats.

För att skriva data till filen kan vi använda funktionen WriteString() på vår temporära fil.

```Go
_, err = tempFile.WriteString("Hello world!")
if err != nil {
	fmt.Println("Error writing to temporary file:", err)
	return
}
```

Och för att läsa data från filen kan vi använda funktionen ReadFile() från paketet "ioutil".

```Go
data, err := ioutil.ReadFile(tempFile.Name())
if err != nil {
	fmt.Println("Error reading from temporary file:", err)
	return
}
fmt.Println("File data:", string(data))
```

## Djupdykning

När vi skapar en temporär fil i Go, skapas den faktiska filen på hårddisken. Den tilldelas också ett unikt namn som genereras av operativsystemet. Det här namnet kan användas för att komma åt filen och läsa/skriva data till den.

När vi stänger filen, raderas den automatiskt från hårddisken. Detta är en avgörande skillnad mellan en temporär fil och en vanlig fil. Om vi behöver behålla filen efter att vårt program har avslutats, måste vi flytta den till en annan plats eller byta namn på den.

Vi kan också ange en specifik mapp där vi vill att vår temporära fil ska skapas. Detta kan göras genom att ange den mappen som det första argumentet till funktionen TempFile(). Om mappen inte finns, kommer Go att skapa den åt oss.

## Se Också

- [Officiell dokumentation för paketet "os"](https://golang.org/pkg/os/)
- [Officiell dokumentation för paketet "ioutil"](https://golang.org/pkg/io/ioutil/)
- [Go Design Patterns: Temporary Files](https://www.ardanlabs.com/blog/2013/11/using-temporary-files-in-go-programming.html)