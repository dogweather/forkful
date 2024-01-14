---
title:                "Kotlin: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 왜
웹 페이지를 다운로드하려는 이유는 여러 가지가 있습니다. 예를 들어, 웹 페이지를 오프라인에서 볼 수 있게 하기 위해서거나, 웹 페이지에 접근하는 데 필요한 데이터를 수집하기 위해서일 수 있습니다.

## 사용 방법
아래에 있는 코드 블록을 사용하여 간단한 방법으로 웹 페이지를 다운로드할 수 있습니다.

```Kotlin
fun main() {
    val url = "https://www.example.com"
    val html = downloadWebPage(url)
    println(html)
}

fun downloadWebPage(url: String): String {
    val connection = URL(url).openConnection() as HttpURLConnection
    connection.requestMethod = "GET"
    return connection.inputStream.use { it.reader().readText() }
}
```

위의 예제 코드에서는 `downloadWebPage()` 함수를 사용하여 웹 페이지를 다운로드하고 해당 페이지의 HTML 내용을 출력합니다. 사용자는 `url` 변수에 다운로드하고자 하는 웹 페이지의 주소를 입력하면 됩니다.

## 깊은 곳으로
코드 예제에서는 `URLConnection`을 사용하여 웹 페이지를 다운로드합니다. 이 클래스는 HTTP 요청을 보내고 응답을 받아오는 기능을 제공합니다. 그리고 `InputStream`을 사용하여 다운로드한 웹 페이지의 내용을 읽어 옵니다.

`URLConnection`의 `openConnection()` 메서드는 `URLConnection`의 인스턴스를 반환하며, 다운로드하려는 웹 페이지의 주소를 파라미터로 전달합니다. 그리고 `inputStream` 속성을 사용하여 다운로드한 내용을 `InputStream` 타입으로 얻을 수 있습니다.

## 참고
- [Kotlin 공식 홈페이지](https://kotlinlang.org/)
- [URLConnection documentation](https://developer.android.com/reference/java/net/URLConnection)
- [Download a URL Content Using Java](https://wls.wwsleep.com/websleepdev/article/java/android-download-url-page)
- [Kotlin Programming: The Big Nerd Ranch Guide](https://www.amazon.com/Kotlin-Programming-Nerd-Ranch-Guide/dp/0135161630)

## 참조하기
- [Kotlin의 함수](https://github.com/Kotlin/kotlinx.coroutines/blob/master/coroutines-guide.md#functions)
- [Kotlin의 읽기 가능한 확장 함수](https://kotlinlang.org/docs/reference/properties.html)
- [Kotlin 프로그래밍의 기본](https://www.amazon.co.jp/Kotlin-%E5%9F%BA%E6%9C%AC-Cientic-%E7%B7%A8%E8%91%97%E6%89%80/dp/4302240489/)