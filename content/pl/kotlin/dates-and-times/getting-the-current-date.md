---
title:                "Pobieranie aktualnej daty"
date:                  2024-02-03T19:10:18.587367-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie aktualnej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?
W programowaniu, uzyskanie aktualnej daty to podstawowe zadanie, które umożliwia programistom dostęp, wyświetlanie lub manipulowanie bieżącą datą w ich aplikacjach. Ta możliwość jest kluczowa dla wszystkiego, od logowania i znakowania czasu zdarzeń po obliczenia na podstawie dat.

## Jak to zrobić:

### Korzystając ze standardowego Kotlina
Kotlin nie posiada własnego API do dat i czasu, ale polega na bibliotece standardowej Javy dla tej funkcjonalności. Oto jak można uzyskać bieżącą datę:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dzisiejsza data: $today")
}
```

**Przykładowy wynik:**
```
Dzisiejsza data: 2023-04-05
```

### Korzystając z java.util.Date
Dla operacji, które wymagają zarówno daty, jak i czasu, możesz woleć `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("Aktualna data i czas: $currentDate")
}
```

**Przykładowy wynik:**
```
Aktualna data i czas: Wed Apr 05 15:20:45 GMT 2023
```

### Korzystając z biblioteki Joda-Time
Przed wprowadzeniem nowego API do daty i czasu w Java 8, Joda-Time był de facto standardem dla operacji związanych z datą i czasem w Javie oraz Kotlinie. Mimo że dla wielu projektów nie jest już konieczny, niektóre mogą nadal go używać z powodu dziedziny lub osobistych preferencji.

Dodaj bibliotekę Joda-Time do pliku build.gradle swojego projektu:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dzisiejsza data: $today")
}
```

**Przykładowy wynik:**
```
Dzisiejsza data: 2023-04-05
```

### Korzystając z ThreeTenABP dla Androida
Dla rozwoju aplikacji na Androida, zaleca się użycie portu wstecznego Java Time API za pośrednictwem projektu ThreeTen Android Backport dla wersji przed poziomem API Androida 26.

Dodaj zależność do pliku build.gradle swojej aplikacji:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Zainicjuj to w swojej klasie aplikacji:
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

Następnie, możesz użyć tego w ten sposób:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Dzisiejsza data: $today")
}
```

**Przykładowy wynik:**
```
Dzisiejsza data: 2023-04-05
```
