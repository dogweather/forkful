---
title:                "Läsa en textfil"
html_title:           "Kotlin: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
I denna artikel ska vi titta på hur man läser en textfil i Kotlin och varför det är en användbar färdighet att ha. Om du arbetar med datahantering, textbaserade spel eller vill kunna hantera användarinput kan du dra nytta av att kunna läsa information från en textfil.

## Hur man gör
För att läsa en textfil i Kotlin behöver vi först skapa en File-objekt, som representerar den aktuella filen. Vi kan sedan använda en BufferedReader för att läsa informationen från filen. Här är ett exempel på hur det kan se ut:

```Kotlin
val fil = File("min_textfil.txt")

// Skapa en BufferedReader från File-objektet
val reader = BufferedReader(FileReader(fil))

// Läs en rad i taget från filen
var rad = reader.readLine()

while (rad != null) {
    // Gör något med raden
    println(rad)
    
    // Läs nästa rad
    rad = reader.readLine()
}

// Stäng läsaren när vi är klara
reader.close()
```

Först skapar vi en File-objekt med namnet på vår textfil som argument. Sedan skapar vi en BufferedReader från File-objektet för att kunna läsa informationen från filen. I while-loopen läser vi en rad i taget från filen tills det inte finns några fler rader att läsa. Vi kan sedan göra något med raden, i detta fall skriver vi ut den till konsolen. Till sist stänger vi läsaren när vi är färdiga.

## Djupdykning
Det finns flera sätt att läsa en textfil i Kotlin, men metoden som vi har visat här är kanske den enklaste och mest användbara. Det finns också andra Java-baserade metoder som man kan använda, till exempel Scanner-klassen eller InputStream-klassen.

Något som är viktigt att tänka på när man läser en textfil är att kolla efter eventuella undantag, till exempel om filen inte kan hittas eller om läsningen misslyckas av andra skäl. Detta kan man åstadkomma med try-catch-block och exception-hantering.

Nu när du vet hur man läser en textfil i Kotlin bör du kunna hantera enklare datahantering eller implementera användarinput i dina projekt. Om du vill läsa mer om konceptet bakom att läsa en fil kan du kolla in den här engelska artikeln från Kotlin's officiella dokumentation: [Reading files in Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/reading-files.html) 

## Se även
Här är några andra artiklar som kan vara intressanta för dig som vill lära dig mer om Kotlin:

- [Introduction to Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/introduction.html)
- [Working with strings in Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/strings.html)
- [Handling exceptions in Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/exceptions.html)