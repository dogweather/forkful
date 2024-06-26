---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:41.250459-07:00
description: "\uBC29\uBC95: Kotlin\uC740 Jsoup\uACFC \uAC19\uC740 \uB77C\uC774\uBE0C\
  \uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD558\uC5EC HTML \uD30C\uC2F1\uC744 \uAC04\uB2E8\
  \uD558\uAC8C \uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uADF8 \uBC29\uBC95\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.168364-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\uC740 Jsoup\uACFC \uAC19\uC740 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC HTML \uD30C\uC2F1\uC744 \uAC04\uB2E8\uD558\uAC8C \uD569\
  \uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 방법:
Kotlin은 Jsoup과 같은 라이브러리를 사용하여 HTML 파싱을 간단하게 합니다. 다음은 그 방법입니다:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Sample Page</title></head><body><p>This is a test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("제목: $title")  // 출력: 제목: Sample Page

    val pText = doc.select("p").first()?.text()
    println("단락: $pText")  // 출력: 단락: This is a test.
}
```

우리는 제목과 단락 텍스트를 추출했으며, 이는 Jsoup이 할 수 있는 일의 표면을 긁는 것에 불과합니다. 하지만 시작이라도 한 것입니다.

## 심층 탐구:
Kotlin 이전에는 주로 Java가 이 일을 했었고, 종종 어색하게 진행되었습니다. Jsoup은 jQuery와 유사한 접근 방식을 제공함으로써 상황을 전환했습니다. HTML 파싱은 Jsoup만이 아니라 HtmlUnit이나 심지어 regex(권장되지 않음)와 같은 다른 라이브러리들도 존재합니다. Jsoup을 사용하면, 당신의 파싱이 문서의 구조를 존중하도록 보장합니다. 이는 DOM 모델을 사용하여 요소들의 선택과 조작을 가능하게 합니다. 또한 견고합니다 – 가장 지저분한 HTML도 파싱할 수 있습니다.

## 추가 정보:
Jsoup에 대해 더 깊이 탐구하기:

- Jsoup 공식 문서: https://jsoup.org/
- "Kotlin for Android Developers" 책: https://antonioleiva.com/kotlin-android-developers-book/
- Kotlin 프로그래밍 언어 공식 사이트: https://kotlinlang.org/

웹 스크래핑과 파싱에 대한 더 넓은 토론과 튜토리얼:

- Kotlin과 Jsoup을 사용한 웹 스크래핑: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Kotlin과 Jsoup을 사용한 Android에서 HTML 파싱: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
