---
title:                "Kotlin: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara användbart i många olika situationer, såsom att bestämma åldersskillnad mellan två personer eller kontrollera om en deadline har gått ut. Med hjälp av Kotlin, kan du enkelt och effektivt jämföra två datum för att få den information du behöver.

## Så här gör du
För att jämföra två datum i Kotlin, kan du använda metoden `compareTo()` som finns tillgänglig på `LocalDate`-objekt. Metoden tar in ett annat `LocalDate`-objekt som argument och returnerar en negativ, noll eller positiv integer beroende på hur de två datumerna förhåller sig till varandra. Om resultatet är negativt, betyder det att det första datumet är tidigare än det andra. Om resultatet är noll, betyder det att de två datumen är samma. Om resultatet är positivt, betyder det att det första datumet är senare än det andra.

```Kotlin
val dateOne = LocalDate.parse("2021-01-01")
val dateTwo = LocalDate.parse("2021-03-01")

val result = dateOne.compareTo(dateTwo)

println(result) // Output: -60
```

I det här fallet, betyder det negativa resultatet att dateOne (1 januari 2021) är tidigare än dateTwo (1 mars 2021) med 60 dagar.

Du kan också använda `isBefore()` och `isAfter()` metoder för att enkelt kontrollera om ett datum är tidigare eller senare än ett annat.

```Kotlin
val dateOne = LocalDate.parse("2021-01-01")
val dateTwo = LocalDate.parse("2021-03-01")

if (dateOne.isBefore(dateTwo)) {
    println("dateOne is before dateTwo") // Output: dateOne is before dateTwo
}
```

## Djupdykning
När du jämför två datum, är det viktigt att förstå att jämförelsen utgår från Kalendern för den lokala enheten eller JVM-instansen. Det kan leda till olika resultat beroende på vilken tidszon du befinner dig i. Till exempel, om du jämför samma datum i tidzonerna UTC+2 och UTC+4, så kommer resultatet att vara olika.

Du kan också använda metoden `isEqual()` för att kontrollera om två datumen är exakt samma, inklusive tid och tidszon.

```Kotlin
val dateOne = ZonedDateTime.parse("2021-01-01T00:00:00+02:00")
val dateTwo = ZonedDateTime.parse("2021-01-01T00:00:00+04:00")

if (dateOne.isEqual(dateTwo)) {
    println("The dates are the same") // Output: The dates are the same
}
```

## Se också
- [Jämföra datum i Java-programmering](https://www.javatpoint.com/dates-comparison-in-java)
- [Officiell dokumentation för Kotlin datum och tid](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-local-date/)
- [Blogginlägg: Jämföra datum i Kotlin](https://blog.kotlin-academy.com/comparing-dates-in-kotlin-datetime-api-55016f1f1de7)