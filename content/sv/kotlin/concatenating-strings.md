---
title:                "Kotlin: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Att sammanfoga strängar är en vanlig operation i många programmeringsspråk, inklusive Kotlin. Det används för att kombinera flera strängar till en enda lång sträng, vilket kan vara användbart för att skapa dynamisk text i en applikation, såsom meddelanden eller rapporter.

## Hur man gör det
Det finns flera sätt att sammanfoga strängar i Kotlin, och jag ska visa dig här några av dem med hjälp av kodexempel och utmatningar.

Först kan vi använda operatorn `+` för att kombinera två eller flera strängar.

```Kotlin
val förnamn = "Anna"
val efternamn = "Andersson"
val fullständigtNamn = förnamn + efternamn
println(fullständigtNamn)
```

Utmatning: `Anna Andersson`

Vi kan också använda funktionsmetoden `plus()` för att sammanfoga strängar.

```Kotlin
val nummer1 = "10"
val nummer2 = "20"
val summa = nummer1.plus(nummer2)
println(summa)
```

Utmatning: `1020`

Det finns också en mer effektiv metod för att sammanfoga många strängar, och det är genom att använda `StringBuilder` -klassen.

```Kotlin
val livsfilosofier = arrayOf("Carpe", "Diem", "Leva", "Livet")
val byggherre = StringBuilder()
for (livsfilosofi in livsfilosofier) {
    byggherre.append(livsfilosofi)
}
val mening = byggherre.toString()
println(mening)
```

Utmatning: `CarpeDiemLevaLivet`

## Djupdykning
Det är viktigt att notera att i Kotlin är strängar oföränderliga, vilket betyder att när en sträng har skapats kan den inte ändras. Därför kommer varje manipulation av en sträng att skapa en ny sträng istället för att ändra den befintliga strängen. Detta är anledningen till varför `StringBuilder` används för att sammanfoga många strängar, eftersom den kan bygga upp en sträng genom att lägga till delar utan att skapa en ny sträng varje gång.

En annan intressant sak att notera är att vid sammanfogning av icke-String-objekt till en sträng, kommer objektets `toString()` -metod att anropas automatiskt.

## Se även
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [Kotlin StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string-builder/index.html)
- [Java StringBuilder vs String Concatenation Performance](https://webtechie.be/post/2019-02-03-java-stringbuilder-vs-string-concatenation-performance/)