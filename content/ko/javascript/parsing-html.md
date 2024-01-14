---
title:                "Javascript: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## 왜

HTML 파싱을 하는 이유는 웹 개발에서 중요한 요소입니다. HTML은 웹 페이지의 구조를 나타내는 언어이기 때문에 해당 정보를 추출하고 조작하기 위해 파싱이 필요합니다.

## 방법

### 기본적인 HTML 파싱

HTML을 파싱하려면 먼저 해당 웹 페이지의 HTML 코드를 얻어와야 합니다. 이는 `fetch()` 메소드를 사용하거나 `XMLHttpRequest` 객체를 이용하여 수행할 수 있습니다. 그리고 `DOMParser` 객체를 생성한 뒤, `parseFromString()` 메소드를 사용하여 HTML 코드를 파싱할 수 있습니다.

```Javascript
fetch('https://www.example.com')
  .then(response => response.text())
  .then(html => {
    const parser = new DOMParser();
    const doc = parser.parseFromString(html, 'text/html');
    // 파싱된 HTML에 대한 작업 수행
  });
```

### CSS 선택자를 이용한 정보 추출

CSS 선택자를 사용하여 원하는 정보를 추출할 수도 있습니다. 이를 위해 `querySelector()`나 `querySelectorAll()` 메소드를 사용할 수 있습니다. `querySelector()`는 첫 번째로 일치하는 요소를 반환하고, `querySelectorAll()`은 모든 일치하는 요소를 반환합니다.

```Javascript
const title = doc.querySelector('h1').innerText;
console.log(title); // Example 페이지의 h1 요소의 텍스트를 콘솔에 출력
```

### 데이터를 HTML로 출력

파싱한 데이터를 다시 HTML로 출력할 수도 있습니다. 이 때 `innerHTML` 속성을 사용합니다.

```Javascript
const newData = '새로운 데이터';
const p = document.createElement('p');
p.innerText = newData;

doc.body.appendChild(p); // body 요소의 맨 마지막에 p 요소 추가
```

## 깊게 들어가기

HTML 파싱은 보통 스크래핑이나 데이터 마이닝 등의 웹 데이터 관련 작업에서 많이 사용됩니다. 파싱하는 방법에는 여러 가지가 있고, CSS 선택자 이외에도 XPath를 사용하여 원하는 요소를 선택할 수 있습니다. 또한 파싱하기 전에 정규식을 사용하여 필요한 정보를 추출할 수 있습니다.

## 참고자료

- [MDN: Parsing and serializing HTML](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [CSS 선택자 개요](https://developer.mozilla.org/en-US/docs/Learn/CSS/Building_blocks/Selectors)
- [XPath로 HTML 파싱하기](https://developer.mozilla.org/en-US/docs/Web/XPath)
- [정규식을 이용한 데이터 추출](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)