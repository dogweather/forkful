---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지를 다운로드하는 것은 인터넷에서 정보를 추출하거나 저장함으로써 특정 웹 페이지의 내용을 내 PC로 복사하는 것을 의미합니다. 프로그래머들이 이것을 하는 이유는 웹 페이지에 있는 원시 데이터를 얻어 분석하거나, 웹 크롤링을 통해 대량의 정보를 수집하는 등 다양한 목적이 있습니다. 

## 어떻게:

Kotlin을 이용해 웹 페이지를 다운로드하는 과정은 매우 간단합니다. 아래에 예제 코드를 제공하겠습니다.

```Kotlin
import java.io.*
import java.net.URL

fun downloadWebPage(url: String) {
    try {
        val webUrl = URL(url)
        BufferedReader(InputStreamReader(webUrl.openStream())).use { reader ->
            val content = reader.lines().toArray().joinToString("\n")
            println(content)
        }
    } catch (ex: IOException) {
        println("${ex.localizedMessage}")
    }
}

fun main() {
    downloadWebPage("https://www.kotlinlang.org/")
}
```
위 코드를 실행하면 Kotlin 공식 홈페이지의 HTML 코드가 콘솔에 출력됩니다.

## 깊게 파기:

웹 페이지를 다운로드하는 것은 HTTP(HTTP 통신 프로토콜)가 제안된 이후로 많은 발전을 거쳐온 기술입니다. URLSession, HttpClient, Axios 등 다양한 라이브러리와 프레임워크를 통해 간단하게 웹 페이지를 다운로드할 수 있습니다. 하지만 Kotlin에서는 표준 라이브러리만을 사용해도 충분합니다. 위에서 작성한 코드는 매우 기본적인 것으로 실제 프로젝트에서는 예외 처리나 다양한 HTTP 메서드에 대한 대응 등을 고려해야 할 필요가 있습니다.

## 추가로 볼 것들:

웹 크롤링에 대해 더 자세히 알고 싶으시다면, 아래 링크를 참조해 주세요.

- [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/)
- [웹 스크레이핑 활용 가이드](https://jsoup.org/cookbook/extracting-data/working-with-urls)
- [HTTP 및 웹 페이지 다운로드에 대한 자세한 가이드](https://developer.mozilla.org/en-US/docs/Web/HTTP)