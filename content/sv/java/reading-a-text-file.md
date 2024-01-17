---
title:                "Läsa en textfil"
html_title:           "Java: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in en textfil betyder att man laddar in information från en textfil i sitt program. Detta är användbart för att kunna manipulera och använda den informationen i sitt program på olika sätt.

Programmerare utför detta för att kunna läsa in stora mängder data på ett effektivt sätt och kunna använda det i sina program.

## Så här gör du:
Det första steget är att hitta den textfil som man vill läsa in i sitt program. Sedan behöver man öppna och läsa in filen genom att använda klassen "FileReader" och "BufferedReader". Här är ett exempel på hur man skulle kunna läsa in en textfil som innehåller en lista på namn:

```Java
File file = new File("namnlista.txt"); // skapar en File objekt
FileReader reader = new FileReader(file); // öppnar filen för läsning
BufferedReader in = new BufferedReader(reader); // skapar en BufferedReader objekt

String name; // skapar en variabel för att lagra namn från filen

while ((name = in.readLine()) != null) { // loopar genom filen och läser in varje rad
    System.out.println(name); // skriver ut namnet
}

in.close(); // stänger filen efter att den har lästs in
```

I detta exempel öppnar och läser vi in filen "namnlista.txt" och skriver ut varje namn på en egen rad i konsolen. 

## Djupdykning:
Att läsa in textfiler är en grundläggande del av programmering och har funnits sedan de tidiga dagarna av datavetenskapen. Det finns olika sätt att läsa in textfiler, men FileReader- och BufferedReader-metoderna är vanligt förekommande i Java.

Några alternativa sätt att läsa in textfiler är genom att använda klasserna "Scanner" eller "InputStream", men det är viktigt att välja en lämplig metod beroende på filens storlek och innehåll.

Implementationen av inläsningen av en textfil kan se olika ut beroende på vad målet är för inläsningen. Om man vill läsa in en textfil för att lagra och använda informationen i sitt program, kan man behöva använda andra metoder för att hantera filen.

## Se även:
Här är några användbara källor för vidare läsning om att läsa in textfiler i Java:

- [Oracle Java dokumentation om FileReader-klassen](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Tutorialspoint Java tutorial om att läsa in textfiler](https://www.tutorialspoint.com/java/io/java_io_filereader.htm)
- [Baeldung tutorial om att läsa in textfiler i Java](https://www.baeldung.com/java-read-lines-large-file)