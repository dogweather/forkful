---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTML 파싱은 HTML 코드를 분석하고 이해 가능한 구조로 변환하는 과정입니다. 이를 통해 프로그래머들은 웹 페이지의 데이터를 쉽게 추출하고 처리할 수 있습니다.

## 어떻게:

Kotlin에서 HTML 파싱을 수행하려면, jsoup 라이브러리를 사용하면 됩니다. 
```kotlin
// 첫째로, jsoup 라이브러리를 프로젝트에 추가해야 합니다.
dependencies {
    implementation 'org.jsoup:jsoup:1.14.1'
}

// 그런 다음 HTML를 파싱하는 가장 간단한 방법은 다음과 같습니다.

fun main() {
    val html = "<html><head><title>제목</title></head><body>내용</body></html>"
    val doc = Jsoup.parse(html)
    println("Title: " + doc.title())
    println("Body: " + doc.body().text())
}
```
이러한 코드는 '제목'을 프린트하는 데 사용되며, 그렇지 않으면 '내용'을 프린트합니다.

## 깊게 파보기:
   
HTML 파싱은 웹 크롤링의 가장 기본적인 부분으로, 웹의 초기부터 사용되어 왔습니다. 이를 대체할 수 있는 것들로는 DOM 파싱, SAX 파싱, XML 파싱 등이 있으나 이 모든 것들이 HTML 파싱을 완전히 보안할 수 있는 것은 아닙니다. HTML 파싱의 구현 세부 사항은 자신이 사용하는 라이브러리에 따라 다르지만, 대부분의 경우 Document Object Model (DOM)을 기반으로 합니다.

## 참고 자료:

[jsoup 공식 문서](https://jsoup.org/cookbook/extracting-data/selector-syntax)

[HTML 파싱에 대한 위키백과 정보](https://en.wikipedia.org/wiki/HTML_parsing)

[Kotlin 공식 문서](https://kotlinlang.org/docs/home.html)