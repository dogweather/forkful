---
title:                "HTML 파싱"
html_title:           "Swift: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/parsing-html.md"
---

{{< edit_this_page >}}

## 왜
HTML을 파싱하는 이유는 웹에서 데이터를 추출하는 것이 쉽고 편리하기 때문입니다. 예를 들어, 웹 스크래핑을 통해 웹 사이트의 제품 가격을 가져와 비교할 수 있습니다.

## 어떻게
Swift에서 HTML을 파싱하는 방법은 다음과 같습니다:

```Swift
if let url = URL(string: "http://www.example.com") {
    do {
        let html = try String(contentsOf: url, encoding: .utf8)
        let document = try HTML(html: html, encoding: .utf8)
        for header in document.css("h3") {
            print(header.text)
        }
    } catch {
        print("Error parsing HTML: \(error)")
    }
}
```

위 코드는 예제 웹 사이트에서 `<h3>` 태그의 텍스트를 추출합니다. 출력 결과는 다음과 같습니다:

```
Title 1
Title 2
Title 3
```

## 딥 다이브
HTML을 파싱하는 더 깊은 정보를 알아보겠습니다. Swift의 HTML파서인 SwiftSoup은 CSS 선택자를 사용해 HTML 요소를 선택할 수 있도록 지원합니다. 또한, XPath를 사용해 보다 복잡한 HTML 구조를 파싱할 수도 있습니다.

See Also (참고자료):
- [SwiftSoup 공식문서](https://github.com/scinfu/SwiftSoup)
- [CSS 선택자 사용법](https://www.w3schools.com/cssref/css_selectors.asp)
- [XPath 사용법](https://www.w3schools.com/xml/xml_xpath.asp)