---
title:                "Kotlin: Läsa en textfil"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en grundläggande uppgift för många programmerare. Det gör det möjligt att bearbeta och använda information som är lagrad i en enkel och läsbar form. Om du vill lära dig hur man läser en textfil i Kotlin, fortsätt läsa!

## Hur man gör det

För att läsa en textfil i Kotlin behöver du först skapa en File-objekt som representerar din fil. Du kan då använda File-objektets readText () metod för att läsa innehållet i filen som en sträng.

```Kotlin
val fil = Fil("exempel.txt")
val innehåll = fil.readText()
print(innehåll)
```

Om innehållet i din fil är formaterat med rader separerade av "ny rad" -tecken kan du dela upp strängen till en lista med hjälp av split () -metoden.

```Kotlin
val fil = Fil("exempel.txt")
val innehåll = fil.readText()
val rader = innehåll.split("\n")
for (rad in rader) {
    print(rad)
}
```

## Djupdykning

När du läser en textfil i Kotlin finns det några saker du bör tänka på. För det första måste du se till att filen faktiskt finns på den plats som du anger i File-objektet. Om filen inte finns, kommer en FileNotFoundException att kastas.

För det andra är det viktigt att förstå att läsa en textfil är en synkron operation, vilket innebär att all kod som kommer efter att filinnehållet lästs kommer att vänta på att operationen är klar. Om du vill göra flera saker med filinnehållet samtidigt, kan du göra det genom att använda en AsyncTask eller andra trådhanteringsmekanismer.

Slutligen ska man komma ihåg att stänga filen efter läsningen är klar. Detta görs enkelt genom att kalla close () -metoden på File-objektet.

## Se även

Här är några användbara länkar för att lära dig mer om hur man läser en textfil i Kotlin:

- Kotlin.org - Filhantering: https://kotlinlang.org/docs/tutorials/kotlin-for-py/download-files.html
- Tutorialspoint - Läsning och skrivning av filer med Kotlin: https://www.tutorialspoint.com/kotlin/kotlin_reading_and_writing_files.htm
- Kotlin för Androidutvecklare - Att läsa och skriva filer: https://www.raywenderlich.com/182266/kotlin-for-android-an-introduction