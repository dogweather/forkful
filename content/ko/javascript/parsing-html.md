---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 해야하나?

HTML 파싱은 HTML문서나 문자열을 분석하여 DOM 노드들로 변환하는 과정입니다. 이를 통해 개발자들이 HTML 요소들을 쉽게 조작하고, 데이터를 찾거나, 정보를 추출할 수 있게 해줍니다.

## 어떻게 하나:

HTML 노드를 파싱하는 가장 간단한 예제는 아래와 같습니다.

```Javascript
let parser = new DOMParser();
let doc = parser.parseFromString("<html><body><p>Hello World!</p></body></html>", "text/html");
console.log(doc.body.textContent);
```

이 예제의 출력 값은 "Hello World!" 입니다.

## 깊게 알아보기:

1. **역사적 맥락**: 웹 페이지가 처음으로 개발되었을 때, HTML 파싱은 매우 간단한 작업으로 간주되었습니다. 문법 구조는 단순했고, 화면에 표시할 요소만 존재했습니다. 하지만 웹이 복잡해짐에 따라, 파싱 로직도 복잡해졌고 이를 위한 도구들도 발전되었습니다.

2. **대안들**: 대안으로는 `jQuery`와 같은 라이브러리를 사용할 수 있습니다. 이는 HTML 문자열을 쉽게 파싱하고 요소에 접근할 수 있도록 도와줍니다. 하지만 `DOMParser`는 표준 내장 API이기 때문에 외부 라이브러리 없이도 브라우저에서 사용 가능합니다.

3. **구현에 관한 세부 정보**: HTML 파싱은 글로벌 `DOMParser` 객체를 사용하여 수행될 수 있으며, 이는 브라우저에서 제공하는 표준 API입니다. `parseFromString`메소드를 사용하여 HTML 문자열을 DOM 트리로 변환할 수 있습니다.

## 참고하면 좋을 링크들:

1. [MDN: DOMParser](https://developer.mozilla.org/ko/docs/Web/API/DOMParser)

2. [W3C: Parsing HTML strings](https://www.w3.org/TR/html5/infrastructure.html#parsing-html-strings)

3. [jQuery: Parse HTML](https://api.jquery.com/jquery.parsehtml/)