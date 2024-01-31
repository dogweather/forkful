---
title:                "HTML 파싱"
date:                  2024-01-20T15:32:45.516757-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

category:             "Kotlin"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTML 파싱은 웹 페이지의 구조를 분석하고 그 데이터를 우리가 사용할 수 있는 형태로 바꾸는 과정입니다. 개발자들은 데이터 추출, 웹 스크레이핑, 콘텐츠 마이그레이션 등의 이유로 HTML을 파싱합니다.

## How to: (방법)
```kotlin
// Jsoup 라이브러리를 사용한 예시입니다.
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>가나다 HTML</title></head>" +
               "<body><p>이것은 파싱 예제입니다.</p></body></html>"
    val doc = Jsoup.parse(html)
    
    val title = doc.title()
    println("Title: $title")
    
    val pText = doc.select("p").first().text()
    println("Paragraph text: $pText")
}

// 콘솔에 출력되는 결과:
// Title: 가나다 HTML
// Paragraph text: 이것은 파싱 예제입니다.
```
HTML 문서의 구조를 추출하고, 특정 태그 내용을 가져옵니다. 간단하면서도 강력한 Jsoup 라이브러리를 활용했습니다.

## Deep Dive (심층 분석)
HTML 파싱은 웹의 초기 시절부터 필요했습니다. 원문 데이터를 자동으로 처리하고 필요한 정보를 추출하기 위해 개발되었습니다. 대안으로는 regex(정규 표현식)을 사용하는 방법도 있지만, 크고 복잡한 HTML 문서에는 적합하지 않고 오류가 발생하기 쉽습니다.

Jsoup는 'CSS 선택자'를 사용하여 요소를 간편하게 선택할 수 있으며 내부적으로 DOM 구조를 활용하여 HTML을 파싱합니다. 이 라이브러리는 매우 직관적이며, 크게 HTML을 로드(load)하고, 파싱(parse)하며, 요소를 선택(select)하고, 데이터를 추출(extract)하는 프로세스로 나뉩니다.

## See Also (참조)
- Jsoup 공식 홈페이지: [https://jsoup.org](https://jsoup.org)
- HTML 파싱에 대한 더 깊은 이해: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTML)
- Kotlin 언어 참조: [Kotlin 공식 문서](https://kotlinlang.org/docs/reference/)
