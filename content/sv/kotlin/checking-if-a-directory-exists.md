---
title:    "Kotlin: Kontrollera om en mapp finns"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Varför

I programmering kan det vara viktigt att kontrollera om en viss mapp finns i ett system. Det kan hjälpa till att undvika fel och se till att programmet fungerar korrekt. 

## Hur man gör det

För att kontrollera om en mapp finns i Kotlin kan man använda funktionen `exists()` från klassen `File`. Den tar in en sträng som argument, vilket är sökvägen till mappen som man vill kontrollera. Om mappen finns returnerar den `true`, annars returneras `false`.

```Kotlin
val directory = File("C:\\Users\\Username\\Documents\\KotlinProject")
val exists = directory.exists()

println("Mappen finns: $exists")
```

I koden ovan skapar vi först en instans av klassen `File` med sökvägen till vår mapp. Sedan anropar vi `exists()` på vår instans och sparar resultatet i en variabel som sedan skrivs ut.

## Djupdykning

En intressant aspekt av att kontrollera om en mapp finns är att det inte bara handlar om att skydda mot fel. Det kan också vara användbart i andra situationer, som att kontrollera om en användare har tillgång till en viss mapp innan man försöker läsa eller skriva till den. 

Man kan även använda sig av andra funktioner i klassen `File` för att interagera med mappen, som att skapa den om den inte finns eller ta bort den om den gör det.

See Also

- [Kotlin dokumentation om klassen `File`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Tutorial om att skapa och ta bort mappar i Kotlin](https://www.baeldung.com/kotlin-create-delete-directory)
- [Mer om filhantering i Kotlin](https://www.youtube.com/watch?v=Z8dk-rpj4gQ)