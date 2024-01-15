---
title:                "HTML 파싱"
html_title:           "Javascript: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

HTML을 파싱하는 것의 이점은, 웹에서 정보를 추출하거나 웹 애플리케이션을 개발하기 위해 필요합니다.

## 방법

HTML을 파싱하는 가장 간단한 방법은 Javascript의 DOM(Document Object Model)을 사용하는 것입니다. 아래의 코드를 참고하세요.

```Javascript
let htmlString = "<html><head><title>My Page<title></head><body><h1>Hello World!</h1></body></html>";
let parsedHTML = new DOMParser().parseFromString(htmlString, "text/html");
console.log(parsedHTML.title); // Output: "My Page"
console.log(parsedHTML.body.children[0].innerHTML); // Output: "Hello World!"
```

위의 코드에서는 DOMParser 객체의 parseFromString 메소드를 사용하여 주어진 문자열을 HTML 문서로 파싱한 뒤, 원하는 정보를 추출할 수 있습니다. 이를 활용하면 웹에서 필요한 데이터를 쉽게 추출할 수 있습니다.

## 깊이 파고들기

HTML 파싱은 웹 개발에서 매우 중요한 부분입니다. DOM을 이용하여 HTML 문서를 파싱하면 웹 애플리케이션에서 동적으로 정보를 가져올 수 있기 때문입니다. 또한, 웹 크롤링이나 스크래핑을 할 때도 HTML 파싱이 중요한 역할을 합니다.

HTML 파싱의 성능을 향상시키기 위해서는, 파싱할 HTML 문서의 구조를 미리 파악하는 것이 중요합니다. 불필요한 DOM 변화를 최소화하고, 적절한 선택자를 이용해 원하는 정보를 추출하는 것이 좋습니다.

## 더 보기

- [DOMParser - MDN](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [Introduction to the DOM - MDN](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction)
- [Parsing HTML With Javascript - Medium](https://medium.com/codex/parsing-html-with-javascript-caf8ca9d84be?source=bookmarks---------0----------------)