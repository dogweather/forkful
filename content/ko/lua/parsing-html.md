---
title:                "HTML 파싱"
html_title:           "Lua: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇인가요? 왜 사용하나요?

HTML 파싱(Parsing)이란 무엇일까요? 단순히 말하면 HTML 문서에서 내용이나 구조를 추출하는 작업을 말합니다. 이를 통해 웹 페이지의 구성요소를 분석하고 데이터를 추출할 수 있습니다. 이런 작업이 필요한 이유는 다양합니다. 예를 들어, 웹 크롤러를 만들 때 웹 페이지의 정보를 수집하기 위해 HTML 파싱이 필수적입니다. 또한 웹 크롤러 외에도 HTML을 변환하거나 분석하는 다양한 용도로 사용됩니다.

## 어떻게 하나요?

```Lua
-- 예제 코드
local html = "<html><head><title>Hello World</title></head><body><p>Hello</p></body></html>"
local parser = require("htmlparser")
local dom = parser.parse(html)
print(dom.head.title.text) -- output: "Hello World"
print(dom.body.children[1].text) -- output: "Hello"
```

위 코드는 Lua에서 HTML 파싱을 하는 방법의 간단한 예제입니다. 우선 `html` 변수에 준비된 HTML 코드를 저장한 뒤, `htmlparser` 모듈을 불러와 해당 코드를 파싱합니다. 파싱된 결과는 `dom` 변수에 담기게 되는데, 이를 통해 원하는 데이터를 추출할 수 있습니다. 예제에서는 `dom.head.title.text`를 통해 `<title>` 태그 안에 있는 텍스트를 가져오고, `dom.body.children[1].text`를 통해 `<body>` 태그의 첫 번째 자식인 `<p>` 태그에 있는 텍스트를 가져옵니다.

## 깊은 이해

HTML 파싱은 웹의 발전과 함께 그 중요성이 커져갔습니다. 1990년대부터 사용되기 시작해 현재까지 많은 웹 기술들이 등장하면서 HTML 파싱 방법 역시 다양해졌습니다. 예를 들어, jQuery 같은 자바스크립트 라이브러리는 HTML 파싱을 도와주는 기능을 가지고 있습니다. 또한, 다양한 언어와 라이브러리에서 HTML 파싱을 지원하고 있습니다. 하지만 Lua에서는 `lhtml` 라이브러리를 통해 파싱을 할 수 있습니다. `lhtml`은 C로 작성된 라이브러리로, 단순하면서도 빠르게 동작하는 장점이 있습니다.

## 더 알아보기

- [HTML 파싱 라이브러리 lhtml](https://github.com/leafo/lhtml)
- [NetSpider - 웹 크롤러 예제](https://github.com/alibaba/net-spider)