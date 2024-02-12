---
title:                "Getting the current date"
aliases:
- /en/kotlin/getting-the-current-date/
date:                  2024-02-03T19:02:48.540827-07:00
model:                 gpt-4-0125-preview
simple_title:         "Getting the current date"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
In programming, getting the current date is a fundamental task that enables developers to access, display, or manipulate the current date within their applications. This capability is crucial for anything from logging and time-stamping events to calculations based on dates.

## How to:

### Using Standard Kotlin
Kotlin does not have its own date and time API, but relies on the Java Standard Library for this functionality. Here's how you can get the current date:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Today's Date: $today")
}
```

**Sample output:**
```
Today's Date: 2023-04-05
```

### Using java.util.Date
For operations that require both the date and time, you might prefer `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("Current Date and Time: $currentDate")
}
```

**Sample output:**
```
Current Date and Time: Wed Apr 05 15:20:45 GMT 2023
```

### Using Joda-Time Library
Before Java 8 introduced a new Date and Time API, Joda-Time was the de-facto standard for date-time operations in Java and Kotlin. Even though it's no longer necessary for many projects, some may still use it for legacy reasons or personal preference.

Add the Joda-Time library to your project's build.gradle file:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Today's Date: $today")
}
```

**Sample output:**
```
Today's Date: 2023-04-05
```

### Using ThreeTenABP for Android
For Android development, using the backport of the Java Time API via the ThreeTen Android Backport Project is recommended for versions before Android API Level 26.

Add the dependency to your app's build.gradle file:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Initialize it in your Application class:
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

Then, you can use it like this:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Today's Date: $today")
}
```

**Sample output:**
```
Today's Date: 2023-04-05
```
