---
title:    "Java: Skriva en textfil"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Varför skriva en textfil

Att skriva en textfil är en viktig del av programmering eftersom det är ett enkelt och effektivt sätt att spara data. Det kan också vara mycket användbart när man behöver dela information mellan olika program.

# Så här skriver du en textfil i Java

För att skriva en textfil i Java behöver du använda "FileWriter" klassen. För att börja, skapa en "FileWriter" objekt som pekar till den önskade filen. Du kan lägga till texten som du vill skriva genom att använda "write" metoden på "FileWriter" objektet. Se till att stänga filen när du är klar.

```
Java
FileWriter fil = new FileWriter("mittDokument.txt");
fil.write("Detta är en exempeltext som kommer att sparas i dokumentet.");
fil.close();
```

För att skapa en ny rad i textfilen kan du använda "newLine" metoden på "BufferedWriter" klassen.

```
Java
BufferedWriter writer = new BufferedWriter(new FileWriter("mittDokument.txt"));
writer.write("Första raden av text.");
writer.newLine();
writer.write("Andra raden av text.");
writer.close();
```

Det är också viktigt att hantera eventuella undantag som kan uppstå när du skriver en textfil. Du kan använda "try-catch" block för att hantera detta.

# Djupdykning i skrivande av textfiler

En textfil kan innehålla olika datatyper, t.ex. strängar, heltal eller andra variabler. Du kan använda "toString" metoden på dessa variabler för att konvertera dem till text innan du skriver dem till filen.

Det finns också olika typer av teckenkodningar som kan användas för att skriva en textfil. Du kan specificera vilken teckenkodning du vill använda när du skapar "FileWriter" objektet.

# Se även

- [Java FileWriter dokumentation](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Java BufferedWriter dokumentation](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedWriter.html)
- [Java try-catch dokumentation](https://docs.oracle.com/javase/tutorial/essential/exceptions/try.html)