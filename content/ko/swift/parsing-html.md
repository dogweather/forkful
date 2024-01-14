---
title:                "Swift: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/parsing-html.md"
---

{{< edit_this_page >}}

# 왜 파싱(Parsing)을 해야 할까요?

우리는 웹에서 다양한 정보를 얻고 싶을 때가 있습니다. 하지만 그 정보는 때때로 HTML 형식으로 포장되어 있습니다. 다행히도, 우리는 Swift를 사용하여 파싱을 할 수 있습니다. 이 글에서는 왜 우리가 파싱을 해야 하는지에 대해 알아보겠습니다.

## 어떻게 하나요?

HTML 파싱은 Swift의 m Fetch 모듈을 통해 쉽게 할 수 있습니다. 먼저, `String`으로부터 `HTMLDocument`를 만듭니다. 다음은 원하는 HTML 요소를 선택합니다. 예를 들어, `<p>` 태그의 내용을 가져오려면 다음의 코드 블록을 사용할 수 있습니다.

```Swift
let htmlStr = "<html><p>Hello, World!</p></html>"
let htmlDocument = try HTML(html: htmlStr, encoding: .utf8)
let pElement = htmlDocument.css("p").first
print(pElement?.text) // "Hello, World!"
```

위 코드는 `"Hello, World!"`라는 결과를 출력합니다. 이 외에도 여러가지 HTML 요소를 선택하고 원하는 내용을 추출할 수 있습니다.

## 더 들어가보기

HTML 파싱을 할 때 주의해야 할 점은 HTML의 형식에 따라 코드를 수정해야 할 수도 있다는 것입니다. 예를 들어, 만약 `<p>` 태그가 여러 개 존재한다면 우리는 `.first` 대신 `.first?`과 같이 선택자를 수정해야 합니다. 또한, HTML 내부에서는 "&"를 "&amp;"라는 HTML 코드로 표현해야 합니다.

# 또 다른 정보 보기

- [SwiftSoup documentation](https://github.com/scinfu/SwiftSoup)
- [HTML parsing with Swift 4](https://www.raywenderlich.com/1485158-html-parsing-with-swift-4)
- [Swift HTML parser with XPath support](https://github.com/robbiehanson/KissXML)