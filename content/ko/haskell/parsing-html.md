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

## 무엇 & 왜?

HTML 구문 분석이란 무엇일까요? 그것은 웹 페이지의 HTML 코드를 읽고 데이터를 추출하는 프로세스를 말합니다. 프로그래머들이 이것을 하는 이유는 웹 사이트에서 필요한 정보를 추출하여 데이터를 분석하고, 가공하고, 사용하기 위해서 입니다.

## 하는 방법:

우리는 ```Haskell ... ``` 코드 블록 내에서 예제 코드와 출력을 나타낼 것입니다. 그러나 직접 해 보기 전에, ```HTML-parser``` 라이브러리를 다운로드하고, 명시적으로 가져와야 합니다.

```Haskell
import Text.HTML.Parser
```

이제, ```parseTags``` 함수를 사용해 HTML 코드를 파싱할 수 있습니다. 예를 들어, 다음과 같이 간단한 HTML 코드를 사용해 봅시다:

```Haskell
let html = "<html><body><h1>Hello World!</h1></body></html>"

let parsed = parseTags html -- [TagOpen "html" [], TagOpen "body" [], TagOpen "h1" [], TagText "Hello World!", TagClose "h1", TagClose "body", TagClose "html"]

```

예제 코드에서 볼 수 있듯이, HTML 코드는 태그를 기준으로 나눠져 있습니다. 이제 우리는 ```parsed``` 리스트에서 필요한 태그와 텍스트를 추출할 수 있습니다.

## 깊게 파고들기:

HTML 구문 분석은 인터넷의 시작과 함께 등장한 고전적인 문제입니다. 예전에는 정규식을 사용해 HTML 코드를 파싱하는 것이 일반적이었습니다. 하지만, 이것은 복잡하고 실수하기 쉬웠기 때문에 ```HTML-parser``` 라이브러리와 같은 도구가 도입되었습니다.

대안으로, 최근에는 ```tagsoup```이라는 라이브러리가 인기를 얻고 있습니다. 이것은 더 유연하게 HTML 코드를 파싱할 수 있고, 어떤 종류의 HTML 코드든 처리할 수 있습니다.

코드를 더 깊이 들여다보면, HTML 코드를 파싱하는데 사용되는 state machine 등의 다양한 논리를 볼 수 있습니다. 하지만, 이러한 세부 사항은 개발자의 관심사가 아니기 때문에 넘길 수 있습니다.

## 더 알아보기:

- [Text.HTML.Parser documentation](https://hackage.haskell.org/package/html-parser/docs/Text-HTML-Parser.html)
- [HTML-parser Hackage page](https://hackage.haskell.org/package/html-parser)
- [tagsoup library official website](http://hackage.haskell.org/package/tagsoup)