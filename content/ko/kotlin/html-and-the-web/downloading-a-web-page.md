---
date: 2024-01-20 17:44:24.989998-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Kotlin\uC5D0\uC11C\
  \ `URL` \uD074\uB798\uC2A4\uC640 `readText` \uD568\uC218\uB97C \uC0AC\uC6A9\uD574\
  \uC11C \uC6F9 \uD398\uC774\uC9C0\uC758 HTML\uC744 \uAC00\uC838\uC62C \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uAC04\uB2E8\uD558\uAC8C \uB9CC\uB4E4\uC5B4\uBCF8 \uC608\uC81C\
  \ \uCF54\uB4DC\uC640 \uCD9C\uB825 \uACB0\uACFC\uB97C \uBCF4\uC5EC\uB4DC\uB9B4\uAC8C\
  \uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.911850-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Kotlin\uC5D0\uC11C `URL` \uD074\
  \uB798\uC2A4\uC640 `readText` \uD568\uC218\uB97C \uC0AC\uC6A9\uD574\uC11C \uC6F9\
  \ \uD398\uC774\uC9C0\uC758 HTML\uC744 \uAC00\uC838\uC62C \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## How to: (어떻게 하나요?)
Kotlin에서 `URL` 클래스와 `readText` 함수를 사용해서 웹 페이지의 HTML을 가져올 수 있습니다. 간단하게 만들어본 예제 코드와 출력 결과를 보여드릴게요.

```Kotlin
import java.net.URL

fun downloadWebPage(pageUrl: String): String {
    return URL(pageUrl).readText(charset = Charsets.UTF_8)
}

fun main() {
    val webpageContent = downloadWebPage("http://example.com")
    println(webpageContent) // 웹 페이지 내용 출력
}
```

이 작은 코드 조각은 "http://example.com"에서 HTML 콘텐츠를 다운로드하고 출력합니다. Kotlin에서는 한 줄로 웹 페이지를 가져올 수 있으니 편리하죠!

## Deep Dive (깊이 알아보기)
웹 페이지 다운로드 기능은 90년대 초반 인터넷이 대중화되며 필요해졌다. 조기 버전의 웹 브라우저가 페이지를 로컬로 저장할 수 있게 되면서, 자동화된 웹 페이지 다운로드 수요가 생겼다. `java.net.URL` 클래스는 자바 초기 버전부터 있었으며, Kotlin에서도 이를 사용할 수 있다.

다운로드 방법은 여러 가지가 있다. `HttpURLConnection`, OkHttp, Retrofit 같은 라이브러리를 사용하는 방법도 있다. 각 방법은 그 상황의 요구에 맞게 선택될 수 있다.

`readText`는 내부적으로 스트림을 열어 데이터를 읽는다. 큰 파일이나 느린 연결에서는 `readText` 대신 `readBytes`를 사용하는 것이 좋을 수 있고, 스트림을 직접 처리하여 성능을 최적화할 수도 있다.

## See Also (더 보기)
- Kotlin에서 웹 페이지를 다운로드하는 또 다른 방법: [OkHttp Website](https://square.github.io/okhttp/)
- `java.net.URL` 과 `HttpURLConnection` 사용법: [Oracle Java Tutorials](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
- Kotlin 프로그래밍 학습: [Kotlinlang.org](https://kotlinlang.org/docs/reference/)
