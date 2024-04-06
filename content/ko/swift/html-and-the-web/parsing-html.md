---
date: 2024-01-20 15:33:59.091245-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C:) Swift\uC5D0\uC11C\uB294 `SwiftSoup` \uAC19\
  \uC740 \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB85C HTML\uC744\
  \ \uD30C\uC2F1\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC608\uC2DC\uB97C \uBCF4\uC2DC\
  \uC8E0."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.344400-06:00'
model: unknown
summary: "(\uC5B4\uB5BB\uAC8C:) Swift\uC5D0\uC11C\uB294 `SwiftSoup` \uAC19\uC740 \uC11C\
  \uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB85C HTML\uC744 \uD30C\uC2F1\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## How to: (어떻게:)
Swift에서는 `SwiftSoup` 같은 서드파티 라이브러리로 HTML을 파싱할 수 있습니다. 예시를 보시죠.

```Swift
import SwiftSoup

// HTML 문자열 예시
let html = "<html><head><title>First parse</title></head>" + "<body><p>Parsed HTML into a doc.</p></body></html>"

do {
    // HTML 파싱
    let doc = try SwiftSoup.parse(html)
    // title 요소 내용 추출
    let title = try doc.title()
    // 바디의 텍스트 추출
    let body = try doc.body()?.text()
    print(title) // "First parse" 출력
    print(body!) // "Parsed HTML into a doc." 출력
} catch Exception.Error(let type, let message) {
    print("Message: \(message)")
} catch {
    print("error")
}
```
이렇게 `SwiftSoup`을 사용하면 쉽게 HTML 내용을 읽을 수 있습니다.

## Deep Dive (심층 탐구)
HTML 파싱은 초창기 웹 시절부터 중요했습니다. HTML 문서는 웹의 기본 빌딩 블록인데, 파싱 없이는 머신이 이해할 수 없는 단순 텍스트에 불과합니다.

Swift용 HTML 파싱 라이브러리로는 SwiftSoup 외에도 Kanna나 hpple 같은 옵션이 있습니다. SwiftSoup은 Java의 Jsoup 라이브러리에 영감을 받아 만들어졌으며, HTML 요소를 간단히 조작하고 쿼리할 수 있는 API를 제공합니다.

파싱의 세부사항을 살펴보면, 파서가 DOM (Document Object Model)을 구성하고, 요소의 속성과 텍스트를 추출하는 방식을 이해할 수 있습니다. 또한, 네트워크 호출로 HTML을 직접 가져오는 작업도 필요할 수 있습니다.

XML과 HTML은 유사해 보이지만, HTML은 유효하지 않은 마크업을 자주 포함합니다. SwiftSoup와 같은 라이브러리는 이런 유효하지 않은 마크업을 잘 처리해 줍니다. 성능은 사용하는 라이브러리에 따라 크게 달라질 수 있으니, 사용 전에 잘 고려해야 합니다.

## See Also (참고 자료)
- [SwiftSoup GitHub](https://github.com/scinfu/SwiftSoup)
- [HTML Standard](https://html.spec.whatwg.org/)
- [W3C DOM](https://www.w3.org/DOM/)
- [Kanna GitHub](https://github.com/tid-kijyun/Kanna)
- [hpple GitHub](https://github.com/topfunky/hpple)
