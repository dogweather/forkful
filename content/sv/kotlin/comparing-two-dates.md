---
title:    "Kotlin: Jämförande av två datum"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

##Varför
Att jämföra två datum kan vara en nyttig funktion inom programmering. Det kan hjälpa till med att sortera data, beräkna tidsperioder eller skapa notifikationer baserade på datum. I denna bloggpost kommer vi att utforska hur man jämför två datum i Kotlin och ge en djupare förståelse av denna funktion.

##Så här gör du
För att jämföra två datum i Kotlin kan du använda en inbyggd metod som heter `isBefore`, `isAfter` eller `isEqual`. Dessa metoder finns i Java's `LocalDate` klass som kan importeras för att använda i Kotlin.

```Kotlin
import java.time.LocalDate

fun main(){
    val dateOne = LocalDate.of(2021, 9, 5)
    val dateTwo = LocalDate.of(2021, 9, 10)

    println(dateOne.isBefore(dateTwo)) //kommer att skriva ut true eftersom dateOne är innan dateTwo
    println(dateOne.isAfter(dateTwo)) //kommer att skriva ut false
    println(dateOne.isEqual(dateTwo)) //kommer att skriva ut false
}
```

I detta exempel skapar vi två LocalDate objekt och använder sedan de inbyggda metoderna för att jämföra dem. Du kan även använda dessa metoder med variabler som tilldelats olika datum och tider, inte bara för att jämföra med nuvarande datum.

```Kotlin
val currentDate = LocalDate.now()
val specificDate = LocalDate.of(2021, 12, 25)
val specificTime = LocalTime.of(18, 30)

println(currentDate.isAfter(specificDate)) //kommer att skriva ut true eftersom currentDate är efter specificDate
println(specificTime.isBefore(currentDate)) //kommer att skriva ut false
```

##Djupdykning
Vid jämförelse av datum är det viktigt att förstå att det finns flera aspekter att beakta. Först och främst kommer `isBefore` och `isAfter` att jämföra både datum och tid, medan `isEqual` endast kommer att jämföra datum. Om tid också behöver beaktas i jämförelsen bör `isEqual` inte användas.

En annan viktig faktor att tänka på är hur formatet på datumet kan påverka jämförelsen. Om du till exempel har ett datum som är i formatet "dd/mm/yyyy" och ett annat som är i formatet "dd-mm-yyyy" kan de inte jämföras korrekt. Det är viktigt att se till att båda datum är i samma format för att få en korrekt jämförelse.

##Se även
- [Officiell Kotlin dokumentation om LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/index.html)
- [Javas LocalDate klassdokumentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutorial om hur man använder LocalDate i Kotlin](https://www.baeldung.com/kotlin-localdate)