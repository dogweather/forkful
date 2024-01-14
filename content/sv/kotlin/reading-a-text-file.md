---
title:                "Kotlin: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil kan vara en grundläggande del av många programmeringsprojekt. Genom att läsa en textfil kan du lättare analysera data, spara information eller till och med interagera med andra program. Så låt oss ta en titt på hur man kan läsa en textfil med hjälp av Kotlin!

## Hur man gör

För att läsa en textfil i Kotlin behöver du först definiera en `File`-variabel som pekar på den specifika filen du vill läsa. Detta kan göras med hjälp av `File`-klassen, som har en konstruktor som tar emot en sträng som representerar sökvägen till filen. Så här kan det se ut:

```Kotlin
val file = File("minTextfil.txt")
```

Nästa steg är att öppna filen för läsning. För att göra det behöver vi skapa en instans av `BufferedReader`-klassen och använda `FileReader` för att öppna filen. Så här kan det se ut:

```Kotlin
val bufferedReader = BufferedReader(FileReader(file))
```

Sedan, för att faktiskt läsa innehållet i filen, behöver vi använda `readLine()`-metoden på vår `bufferedReader`-variabel. Denna metod läser en rad åt gången och returnerar `null` när den når slutet av filen. Du kan använda en `while`-loop för att läsa alla rader i filen på följande sätt:

```Kotlin
var line = bufferedReader.readLine()
while (line != null) {
    println(line)
    line = bufferedReader.readLine()
}
```

Detta kommer att skriva ut varje rad i filen till konsolen. Du kan naturligtvis göra vilken behandling du vill med varje rad beroende på dina behov.

## Djupdykning

Nu när vi har täckt det grundläggande, låt oss ta en närmare titt på vad som faktiskt händer bakom kulisserna när vi läser en fil. När vi använder `BufferedReader` och `FileReader` i kombination, använder vi faktiskt en teknik som kallas *buffring*. Detta innebär att data i filen läses in i en innehållsförteckning i minnet och sedan läses bit för bit när det behövs. Detta är mycket effektivare än att läsa hela filen på en gång, särskilt när filen är väldigt stor.

Vi använder också *exceptions* när vi läser filer. En exception är ett fel som uppstår i programmet och som kan hanteras på olika sätt. Om du till exempel försöker läsa en fil som inte finns, kommer en `FileNotFoundException` att kastas, vilket innebär att filen inte kunde hittas. Detta ger dig möjlighet att hantera felet på ett lämpligt sätt, till exempel genom att skriva ett meddelande till användaren eller försöka läsa en annan fil.

## Se även

- [Kotlin documentation for File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Kotlin documentation for BufferedReader](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
- [Kotlin documentation for FileNotFoundException](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file-not-found-exception/)