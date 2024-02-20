---
date: 2024-01-20 17:37:02.474611-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC740 \uB0A0\uC9DC \uB370\uC774\uD130\uB97C \uD14D\uC2A4\uD2B8 \uD615\uD0DC\
  \uB85C \uD45C\uD604\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uC774 \uACFC\uC815\
  \uC740 \uB0A0\uC9DC\uB97C \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uBCF4\uC5EC\uC8FC\uAC70\
  \uB098 \uD30C\uC77C\uC5D0 \uC800\uC7A5\uD560 \uB54C \uD544\uC694\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.100754
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\
  \uC740 \uB0A0\uC9DC \uB370\uC774\uD130\uB97C \uD14D\uC2A4\uD2B8 \uD615\uD0DC\uB85C\
  \ \uD45C\uD604\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uC774 \uACFC\uC815\uC740\
  \ \uB0A0\uC9DC\uB97C \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uBCF4\uC5EC\uC8FC\uAC70\uB098\
  \ \uD30C\uC77C\uC5D0 \uC800\uC7A5\uD560 \uB54C \uD544\uC694\uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환하는 것은 날짜 데이터를 텍스트 형태로 표현하는 과정입니다. 이 과정은 날짜를 사용자에게 보여주거나 파일에 저장할 때 필요합니다.

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
