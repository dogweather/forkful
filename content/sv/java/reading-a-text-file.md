---
title:    "Java: Läsa en textfil"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa från en textfil är en vanlig uppgift inom Java-programmering och är användbart för att hämta data och processa den i ditt program. Det kan vara användbart för saker som att läsa in en konfigurationsfil eller ett stort dataset.

## Hur man gör det
Här är ett enkelt exempel på hur man kan läsa från en textfil i Java och skriva ut innehållet:
```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class FileReader {
	public static void main(String[] args) {
		// Skapa en ny fil med sökvägen till den önskade textfilen
		File file = new File("mitt-dataset.txt");
		try {
			// Skapa en Scanner för att läsa från filen
			Scanner scanner = new Scanner(file);
			// Loopa igenom alla rader i filen
			while (scanner.hasNextLine()) {
				// Läs in en rad
				String line = scanner.nextLine();
				// Skriv ut raden
				System.out.println(line);
			}
			// Stäng scannern
			scanner.close();
		} catch (FileNotFoundException e) {
			// Om filen inte kan hittas, skriv ut ett felmeddelande
			System.out.println("Filen kunde inte hittas!");
		}
	}
}
```

Om vi antar att vår textfil innehåller följande rader:
```
Hej
Välkommen till Java-bloggen
Detta är en textfil
```

Så kommer koden ovan att skriva ut följande i konsolen:
```
Hej
Välkommen till Java-bloggen
Detta är en textfil 
```

## Djupdykning
Det finns flera olika sätt att läsa från en textfil i Java, beroende på vad du vill åstadkomma. Du kan till exempel använda BufferedReader för att kunna läsa in texten rad för rad eller använda olika metoder för att processa filinnehållet som att hitta och ersätta textsträngar. Det är också viktigt att komma ihåg att stänga filen när du är klar med att läsa från den för att undvika problem med resursförvaltningen.

## Se även
- [Java File IO tutorial](https://www.javatpoint.com/java-file-io)
- [Java FileReader documentation](https://docs.oracle.com/javase/8/docs/api/java/io/FileReader.html)
- [How to Read a File in Java](https://www.w3schools.com/java/java_files_read.asp)