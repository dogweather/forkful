---
date: 2024-01-20 15:33:59.091245-07:00
description: "HTML \uD30C\uC2F1\uC740 \uC6F9 \uD398\uC774\uC9C0\uC758 \uAD6C\uC870\
  \uC801 \uB370\uC774\uD130\uB97C \uCD94\uCD9C \uBC0F \uC870\uC791\uD558\uB294 \uACFC\
  \uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6F9 \uCF58\
  \uD150\uCE20\uC758 \uB370\uC774\uD130\uB97C \uC77D\uACE0, \uC2A4\uD06C\uB7A9\uD558\
  \uAC70\uB098, \uB2E4\uB978 \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uAE30 \uC704\
  \uD574 \uD30C\uC2F1\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.728775-06:00'
model: unknown
summary: "HTML \uD30C\uC2F1\uC740 \uC6F9 \uD398\uC774\uC9C0\uC758 \uAD6C\uC870\uC801\
  \ \uB370\uC774\uD130\uB97C \uCD94\uCD9C \uBC0F \uC870\uC791\uD558\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC6F9 \uCF58\uD150\
  \uCE20\uC758 \uB370\uC774\uD130\uB97C \uC77D\uACE0, \uC2A4\uD06C\uB7A9\uD558\uAC70\
  \uB098, \uB2E4\uB978 \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uAE30 \uC704\uD574\
  \ \uD30C\uC2F1\uC744 \uD569\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTML 파싱은 웹 페이지의 구조적 데이터를 추출 및 조작하는 과정입니다. 프로그래머들은 웹 콘텐츠의 데이터를 읽고, 스크랩하거나, 다른 형식으로 변환하기 위해 파싱을 합니다.

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
