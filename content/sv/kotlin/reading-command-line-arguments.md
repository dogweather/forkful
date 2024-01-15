---
title:                "Att läsa kommandoradsargument"
html_title:           "Kotlin: Att läsa kommandoradsargument"
simple_title:         "Att läsa kommandoradsargument"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har använt ett program från en kommandorad, har du förmodligen stött på kommandoradsargument. Det är de små kodbitarna som du kan lägga till när du kör ett program för att anpassa dess beteende. I denna artikel kommer vi att ta en djupdykning i hur man läser in kommandoradsargument i Kotlin.

## Så här gör du

Läsning av kommandoradsargument i Kotlin är en enkel process som kan göras med hjälp av `args` parametern i `main()` funktionen. Här är ett exempel:

```Kotlin
fun main(args: Array<String>) {
    println(args[0])
}
```

När detta program körs kommer det att skriva ut det första kommandoradsargumentet som anges vid körning av programmet. Till exempel, om du anger `Hello` som det första argumentet, kommer det att skriva ut `Hello` i terminalen.

Du kan också använda loopar för att läsa in alla kommandoradsargument och utföra olika åtgärder med dem. Här är ett exempel på hur man skriver ut alla argumenten som strängar:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

## Djupdykning

Nu när du har en grundläggande förståelse för hur man läser in kommandoradsargument i Kotlin, låt oss ta en djupare titt på några viktiga begrepp.

Först och främst, `args` parametern i `main()` funktionen är en `Array` av strängar. Det betyder att alla kommandoradsargument är lagrade som strängar och du kan få åtkomst till dem genom att använda index.

Det är också viktigt att notera att ordningen på kommandoradsargumenten är viktig. Det första argumentet som anges i kommandoraden kommer att vara det första elementet i `args` arrayen.

Slutligen, om du vill läsa in annan typ av data från kommandoraden, som t.ex. heltal eller flyttal, måste du använda konverteringsfunktioner för att konvertera strängarna till rätt datatyp.

## Se även

Här är några relaterade artiklar och resurser som kan vara användbara för dig att läsa:

- [Official Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- [Kotlin for Java Developers](https://kotlinlang.org/docs/tutorials/kotlin-for-py/java-interop.html)
- [Kotlin Command Line Tools](https://kotlinlang.org/docs/tutorials/command-line.html)

Tack för att du läste! Om du har några frågor eller feedback kan du gärna lämna en kommentar nedan.