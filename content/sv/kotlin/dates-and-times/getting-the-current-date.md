---
title:                "Få det aktuella datumet"
aliases:
- /sv/kotlin/getting-the-current-date/
date:                  2024-02-03T19:10:15.094121-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få det aktuella datumet"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
I programmering är att få fram dagens datum en grundläggande uppgift som möjliggör för utvecklare att få tillgång till, visa eller manipulera det aktuella datumet inom sina applikationer. Denna funktion är avgörande för allt från loggning och tidsstämpling av händelser till beräkningar baserade på datum.

## Hur man gör:

### Använda Standard Kotlin
Kotlin har inte sitt eget API för datum och tid, utan förlitar sig på Java Standardbiblioteket för denna funktionalitet. Så här kan du få fram dagens datum:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dagens Datum: $today")
}
```

**Exempelutdata:**
```
Dagens Datum: 2023-04-05
```

### Använda java.util.Date
För operationer som kräver både datum och tid kanske du föredrar `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("Aktuellt Datum och Tid: $currentDate")
}
```

**Exempelutdata:**
```
Aktuellt Datum och Tid: Ons Apr 05 15:20:45 GMT 2023
```

### Använda Joda-Time biblioteket
Innan Java 8 introducerade ett nytt API för Datum och Tid, var Joda-Time den de facto-standarden för datum-tidsoperationer i Java och Kotlin. Även om det inte längre är nödvändigt för många projekt, kan vissa fortfarande använda det av skäl som har att göra med äldre system eller personlig preferens.

Lägg till Joda-Time biblioteket i ditt projekts build.gradle-fil:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dagens Datum: $today")
}
```

**Exempelutdata:**
```
Dagens Datum: 2023-04-05
```

### Använda ThreeTenABP för Android
För Android-utveckling rekommenderas att använda bakporten av Java Time API via ThreeTen Android Backport Project för versioner före Android API-nivå 26.

Lägg till beroendet till din apps build.gradle-fil:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Initialisera det i din Application-klass:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Sedan kan du använda det så här:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dagens Datum: $today")
}
```

**Exempelutdata:**
```
Dagens Datum: 2023-04-05
```
