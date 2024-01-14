---
title:    "Arduino: Skapa en tillfällig fil"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Varför?

Att skapa en temporär fil kan vara en användbar teknik för att hantera data eller tillfälliga filer inom ditt Arduino-program. Det kan hjälpa till att organisera ditt arbete och göra din kod mer effektiv.

# Hur man gör det

Skapa en temporär fil är enkelt med Arduino. Genom att använda funktionen `createTempFile()` kan du skapa en temporär fil i operativsystemets temporära filkatalog. Här är ett enkelt kodexempel:

```Arduino
File tempFile = createTempFile();
```

Detta kommer att skapa en temporär fil med ett unikt namn och returnera ett File-objekt som du kan använda för att arbeta med filen.

Om du vill skriva till filen, kan du använda `tempFile.println()` för att lägga till en rad text eller `tempFile.write()` för att skriva ett binärt värde. Här är ett exempel på hur du kan skriva till filen:

```Arduino
tempFile.println("Hello, World!");
tempFile.write(42);
```

För att läsa från filen kan du använda `tempFile.read()` för att läsa ett tecken eller `tempFile.readString()` för att läsa en hel sträng. Ett exempel på hur man läser från filen kan se ut så här:

```Arduino
char character = tempFile.read();
String text = tempFile.readString();
```

När du är klar med att arbeta med filen, se till att stänga den med `tempFile.close()` för att frigöra resurserna.

# Fördjupning

En temporär fil är en fil som endast existerar temporärt och tas bort när den inte längre behövs. Detta kan vara användbart för att hantera temporära data som inte behövs permanent eller för att skapa en temporär backup av en fil som ska bearbetas.

I Arduino-sketchen, kommer `createTempFile()` att använda operativsystemets temporära filkatalog som standard. Men du kan också ange en specifik katalog som ska användas genom att lägga till den som ett argument, till exempel `createTempFile("/tmp")`.

Det finns också andra funktioner som kan vara användbara när du arbetar med temporära filer, som `tempFile.size()` för att kolla storleken på filen eller `tempFile.rename()` för att ändra filnamnet.

# Se även

- [Arduino Reference: createTempFile()](https://www.arduino.cc/reference/sv/language/functions/file-io/tempfile/create-tempfile/)
- [Arduino Reference: File](https://www.arduino.cc/reference/sv/language/functions/file-io/file/)
- [Arduino Reference: String](https://www.arduino.cc/reference/sv/language/variables/data-types/string/)