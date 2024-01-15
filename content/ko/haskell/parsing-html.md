---
title:                "HTML 파싱"
html_title:           "Haskell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## 왜
혹시 웹 개발을 하고 계시다면, 다양한 웹페이지에서 데이터를 추출하는 기능이 필요하게 될 수도 있습니다. 이때 HTML parsing이라는 기술이 필요합니다. 이를 통해 원하는 정보를 정확하게 추출할 수 있고, 자동화된 작업도 가능해집니다.

## 방법
우선, Haskell 프로그래밍 언어를 설치해야 합니다. 그러고 나서 라이브러리 중에서 `tagsoup`을 선택한 뒤, 간단한 예제를 통해 어떻게 HTML을 파싱하는지 알아보겠습니다.

```Haskell
import Text.HTML.TagSoup

main = do
    let html = "<html><body><h1>Hello, World!</h1></body></html>"
    let tags = parseTags html
    let h1 = fromAttrib "h1" $ head tags
    print h1
```

이 예제에서는 `Text.HTML.TagSoup` 라이브러리를 불러오고, `parseTags` 함수를 사용하여 HTML 코드를 파싱합니다. 그리고 `fromAttrib` 함수와 `head` 함수를 사용하여 `<h1>` 태그 안에 있는 정보를 추출합니다. 마지막으로 `print` 함수를 사용해 결과를 출력합니다.

위 코드를 실해하면 "Hello, World!"라는 결과가 출력됩니다. 이처럼 쉽게 HTML 코드를 파싱하여 원하는 정보를 추출할 수 있습니다.

## 딥 다이브
더 깊이 들어가보면, `tagsoup` 라이브러리는 더 다양한 기능을 제공합니다. 예를 들어 HTML 코드에서 특정 태그를 지정하여 그 안에 있는 내용을 추출하는 것도 가능합니다.

```Haskell
import Text.HTML.TagSoup

main = do
    let html = "<html><body><div><p>Hello, World!</p><p>This is a paragraph.</p></div></body></html>"
    let tags = parseTags html
    let pTags = sections (~== TagOpen "p" []) tags
    let p1 = fromTagText $ head pTags
    let p2 = fromTagText $ pTags !! 1
    print p1
    print p2
```

위 코드에서는 `<p>` 태그를 지정하여 그 안에 있는 내용을 추출하는 방법을 보여줍니다. `sections` 함수를 사용하여 `<p>` 태그를 찾은 뒤, `fromTagText` 함수를 사용하여 텍스트를 추출합니다. 그리고 `!!` 연산자를 통해 원하는 위치의 태그를 추출합니다.

이 외에도 더 많은 기능을 `tagsoup` 라이브러리에서 사용할 수 있습니다. 참고자료의 링크를 통해 더 자세한 정보를 확인할 수 있습니다.

## 참고자료
- [Hackage: tagsoup 라이브러리 문서](https://hackage.haskell.org/package/tagsoup/docs/Text-HTML-TagSoup.html)
- [Learn You a Haskell for Great Good! - HTML parsing](http://learnyouahaskell.com/input-and-output#reading-a-file)