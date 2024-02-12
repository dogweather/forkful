---
title:                "Converting a date into a string"
aliases:
- /en/kotlin/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:01.088210-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a date into a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string means representing a specific moment in a human-readable format. Programmers do it to display dates to users or to serialize them for storage and data transfer.

## How to:
In Kotlin, you can convert a `Date` to a `String` using the `SimpleDateFormat` class. Let's roll some code:

```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date() // Create a Date object for the current time
    val format = SimpleDateFormat("yyyy-MM-dd HH:mm:ss") // Define the date pattern
    val dateString = format.format(date) // Convert Date to String
    println(dateString) // Output the date string
}
```

Sample output might look like this:

```
2023-03-25 14:45:32
```

## Deep Dive
Before `java.time` stepped onto the scene, `SimpleDateFormat` was the go-to guy for date-string transformations in Java and, by inheritance, in Kotlin. Yep, Kotlin runs on the Java Virtual Machine and interacts comfortably with Java libraries.

With Java 8, however, `java.time` entered the picture, bringing `DateTimeFormatter` with a much more refined API. This was a game-changer, offering safer, immutable, and thread-safe date-time manipulation. Kotlin's native support for this is seamless:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDateTime.now() // Get current date and time
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formattedDate = currentDate.format(formatter)
    println(formattedDate)
}
```

Alternatives? Sure. For non-standard requirements or juggling between multiple date libraries, third-party options like Joda-Time used to be the golden standard. These days, `java.time` covers most bases.

As per implementation details, `SimpleDateFormat` isn't thread-safe, which means it can trip over its laces when used in concurrent settings. `DateTimeFormatter` doesn't have that issue. Create once, use forever—or at least throughout your application without fretting much.

## See Also
- `DateTimeFormatter` JavaDoc for all your pattern needs: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- If you're feeling nostalgic or need examples for legacy systems, here's the scoop on `SimpleDateFormat`: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
