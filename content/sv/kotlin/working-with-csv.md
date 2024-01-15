---
title:                "Arbeta med csv"
html_title:           "Kotlin: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV-filer, eller Comma Separated Values, är en populär format för att lagra och hantera data. Det är ett lättläst format för både människor och datorer, vilket gör det till en föredragen metod för att lagra och dela information. Genom att använda Kotlin för att arbeta med CSV-filer kan du effektivt hantera och analysera stora datamängder.

## Så här gör du

För att arbeta med CSV-filer i Kotlin behöver du först importera CSV-biblioteket. Det kan du göra genom att lägga till följande kod i din build.gradle fil:

```Kotlin
implementation 'com.opencsv:opencsv:5.0'
```

När biblioteket är importerat kan du använda dess funktioner för att läsa och skriva till CSV-filer. Nedan är ett exempel på hur du kan läsa in en CSV-fil och få ut dess innehåll i en lista av rader:

```Kotlin
import au.com.bytecode.opencsv.CSVReader

fun main() {
  val reader = CSVReader(FileReader("file.csv"))

  var row: Array<String>?
  while ((row = reader.readNext()) != null) {
    println(Arrays.toString(row))
  }

  reader.close()
}
```

Detta kodblock visar också hur du kan använda OpenCSV-biblioteket för att läsa en CSV-fil rad för rad och skriva ut dess innehåll. Vid varje iteration av while-loopen skrivs raden ut som en array av strängar, med varje element motsvarande ett fält i CSV-filen.

För att skriva till en CSV-fil med hjälp av Kotlin kan du använda följande kod:

```Kotlin
import au.com.bytecode.opencsv.CSVWriter

fun main() {
  val writer = CSVWriter(FileWriter("file.csv"))

  val record = arrayOf("Apple", "Banana", "Orange")

  writer.writeNext(record)

  writer.close()
}
```

I det här exemplet används CSVWriter-funktionen för att skapa en ny fil och skriva en ny rad innehållande frukter. Precis som vid läsning av en CSV-fil, måste du stänga writer-objektet när du är klar för att säkerställa att alla datan sparas.

## Djupdykning

Arbetet med CSV-filer kräver att du förstår deras struktur. I en CSV-fil är varje rad en post och varje fält inom en rad separeras av ett kommatecken. Du kan också välja att filen ska ha en rubrikrad som specifierar namnen på varje fält.

En viktig färdighet vid arbetet med CSV-filer i Kotlin är att hantera felaktig data. Om ett fält i en CSV-fil exempelvis innehåller ett kommatecken, kommer det att försvåra läsningen av filen, eftersom kommatecken vanligtvis används som separeringsmarkör. För att hantera detta och andra problem med felaktig data kan du använda OpenCSV:s CSVParser-funktion.

För en mer detaljerad guide till arbetet med CSV-filer i Kotlin, rekommenderar vi att du tittar på dokumentationen för OpenCSV-biblioteket och dess olika funktioner.

## Se även

- [OpenCSV dokumentation](http://opencsv.sourceforge.net/)

- [Kotlin officiell hemsida](https://kotlinlang.org/