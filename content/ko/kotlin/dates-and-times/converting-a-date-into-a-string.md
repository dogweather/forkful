---
date: 2024-01-20 17:37:02.474611-07:00
description: "How to: (\uBC29\uBC95) Kotlin\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBB38\uC790\
  \uC5F4\uB85C \uBCC0\uD658\uD558\uB824\uBA74 `SimpleDateFormat`\uC744 \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4. \uC544\uB798 \uCF54\uB4DC\uC640 \uAC19\uC774 \uC0AC\uC6A9\uD558\
  \uBA74 \uB429\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.188833-06:00'
model: gpt-4-1106-preview
summary: "Kotlin\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\
  \uD558\uB824\uBA74 `SimpleDateFormat`\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## How to: (방법)
Kotlin에서 날짜를 문자열로 변환하려면 `SimpleDateFormat`을 사용합니다. 아래 코드와 같이 사용하면 됩니다.

```Kotlin
import java.text.SimpleDateFormat
import java.util.*

fun main() {
    val date = Date()
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val dateString = formatter.format(date)
    
    println(dateString) // 예제 출력: "2023-04-01 12:30:45"
}
```

## Deep Dive (심화 탐구)
날짜에서 문자열로의 변환은 컴퓨터 프로그래밍 초기부터 있던 필수적인 작업입니다. `SimpleDateFormat`은 Java에 등장한 이래로 많이 사용돼왔고 Kotlin에서도 여전히 사용됩니다. 대안으로는 Java 8부터 추가된 `java.time` 패키지 속의 `DateTimeFormatter`가 있습니다. 이것은 더욱 안전하고 유연한 API를 제공합니다. 하지만 안드로이드 개발에서는 Java 8 기능을 지원하지 않는 구버전 기기도 고려해야 하므로, `SimpleDateFormat`을 사용하는 경우가 많습니다. 

`SimpleDateFormat`은 스레드에 안전하지 않기 때문에, 여러 스레드에서 동일한 인스턴스를 공유해서는 안 됩니다. 각 스레드에서 별도의 인스턴스를 생성하거나, 필요하다면 ThreadLocal을 사용하여 해결할 수 있습니다.

```Kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val current = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formatted = current.format(formatter)
    
    println(formatted) // 예제 출력: "2023-04-01 12:30:45"
}
```

## See Also (참고 자료)
- [SimpleDateFormat 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [DateTimeFormatter 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
