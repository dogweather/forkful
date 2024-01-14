---
title:                "Java: Skapa en temporär fil"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa en temporär fil kan vara användbart i många situationer i din Java-programmering. Det kan hjälpa till att hantera data, utföra databehandling eller spara tillfälliga resultat. Det kan också användas för att förbättra prestandan genom att minska belastningen på hårddisken eller minnet.

## Hur man gör
För att skapa en temporär fil i Java, kan du använda klassen **File** och dess metod **createTempFile()**. Här är ett exempel på hur du kan skapa en temporär fil och sedan skriva till den:
```Java
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class TempFileExample {
    public static void main(String[] args) {
        try {
        	// Skapa en temporär fil
            File tempFile = File.createTempFile("myTempFile", ".txt");

            // Skriv till filen
            FileWriter writer = new FileWriter(tempFile);
            writer.write("Detta är en temporär fil.");
            writer.close();

            System.out.println("Temporär fil skapad: " + tempFile.getAbsolutePath());
            
            // Ta bort filen när programmet avslutas
            tempFile.deleteOnExit();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Output:
```
Temporär fil skapad: C:\Users\user\AppData\Local\Temp\myTempFile7251419664026707538.txt
```
Alternativt kan du också ange en specifik sökväg där du vill att den temporära filen ska skapas:
```Java
File tempFile = File.createTempFile("myTempFile", ".txt", "C:\\temp\\");
```

## Djupdykning
När du skapar en temporär fil i Java, skapas den med ett unikt namn och en unik förlängning för att undvika konflikter med andra filer. Filen kommer också att skapas i det temporära filsystemet på datorn, vilket är ett område som används för temporära filer. När du avslutar programmet, kommer filen automatiskt att raderas från detta filsystem.

En annan viktig sak att tänka på är att varje gång du kör programmet, kommer en ny temporär fil att skapas med ett unikt namn. Detta innebär att du inte kan räkna med att en viss temporär fil alltid kommer att finnas tillgänglig för ditt program. Se även till att ta bort filen när du är klar med den för att inte belasta det temporära filsystemet.

## Se även
- [Java File-klassens dokumentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Skapa temporära filer i Java](https://www.baeldung.com/java-temporary-file)