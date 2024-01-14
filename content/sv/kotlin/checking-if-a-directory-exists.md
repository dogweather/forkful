---
title:                "Kotlin: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Varför

Har du någonsin undrat hur man kan kontrollera om en mapp finns i ditt program? I den här bloggposten kommer vi att gå igenom hur man gör det med hjälp av Kotlin. Det här är en användbar teknik för att skapa mer dynamiska program som är anpassade till olika användares behov.

# Hur man gör det

För att kontrollera om en mapp finns måste vi först importera File-klassen från java.io-paketet.

```Kotlin
import java.io.File
```

Sedan kan vi använda funktionen "exists()" för att kontrollera om en mapp finns i den sökväg som anges.

```Kotlin
val directory = File("min/mapp/sökväg")
if (directory.exists()){
    println("Mappen finns!")
} else {
    println("Mappen finns inte!")
}
```

Om mappen finns så kommer "Mappen finns!" att skrivas ut i konsolen. Annars kommer "Mappen finns inte!" att skrivas ut.

# Djupdykning

Det finns flera saker att tänka på när man kontrollerar om en mapp finns. En viktig sak är att sökvägen som anges i koden är i det absoluta formatet. Detta innebär att den börjar med enhetsbokstav (t.ex. C:\) eller en rotkatalog (/ på Linux och macOS). Om sökvägen är relativ, dvs. en sökväg som börjar från den aktuella arbetskatalogen, måste vi först konvertera den till ett absolut format innan vi använder funktionen "exists()".

En annan sak att tänka på är att funktionen "exists()" kan även returnera värdet "false" om det finns ett namnkonflikten i sökvägen. Detta innebär att det kan finnas en fil eller en mapp med samma namn som den sökväg vi försöker kontrollera. Det är därför viktigt att döpa filer och mappar på ett sätt som undviker namnkonflikter.

# Se också

Här är några andra användbara resurser för att lära dig mer om att kontrollera om en mapp finns i Kotlin:

- [Kotlin dokumentation för File-klassen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-file/index.html)
- [Java.io-paketets dokumentation](https://docs.oracle.com/javase/7/docs/api/java/io/package-summary.html)
- [Tutorialspoint's guide till att använda File-klassen i Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_files.htm)