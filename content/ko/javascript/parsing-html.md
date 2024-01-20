---
title:                "HTML 파싱"
date:                  2024-01-20T15:32:28.765238-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
HTML 파싱은 HTML 데이터를 읽고 그 구조를 이해하는 과정이다. 개발자들은 데이터를 추출, 조작, 저장하기 위해 이 작업을 한다.

## How to: (어떻게 하나요?)
```javascript
const parser = new DOMParser();
const htmlString = `<div>Hello, <b>World!</b></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');

// 요소 접근 및 데이터 추출
const div = doc.querySelector('div');
console.log(div.innerHTML); // 출력: Hello, <b>World!</b>
```

## Deep Dive (심층 분석)
웹 초기에는 HTML 파싱이 주로 서버에서 이루어졌다. Node.js 등장으로 자바스크립트도 서버사이드 파싱이 가능해졌다. 돔파서(DOMParser), 뷰티풀 수프(Beautiful Soup; 파이썬) 같은 도구가 있으며, 이들은 성능과 사용 편의성이 각자 다르다. DOMParser는 브라우저에 내장된 API로, HTML/XML 문서를 파싱한다. 내부적으로는 DOM 트리를 만들며, 결과는 문서 객체로 반환된다. 이해하기 쉽고 빠르다는 장점이 있다.

## See Also (참고 자료)
- MDN Web Docs의 [DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [Node.js cheerio](https://cheerio.js.org/) – 서버사이드에서 사용하는 jQuery식 파서
- W3C의 [HTML5 파싱 규격](https://www.w3.org/TR/html5/syntax.html#parsing)