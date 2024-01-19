---
title:                "문자열 대문자화"
html_title:           "Javascript: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 문자열 대문자화란 무엇인가 & 왜 필요한가?

문자열 대문자화란 코드상의 모든 소문자를 대문자로 바꾸는 작업입니다. 자바스크립트에서 우리는 이 작업을 많이 하는데, 이유는 가독성과 통일성을 높이기 위해서입니다.

## 어떻게 사용하는가:

```Javascript
let msg = "안녕하세요, 자바스크립트!";
let msgUpper = msg.toUpperCase();
console.log(msgUpper);
```
출력 결과:
```Javascript
"안녕하세요, 자바스크립트!"
```

## 심층분석

### 역사적 배경
ES5에서 소개된 `toUpperCase()` 메서드는 문자열을 다루는데 필요한 기본 도구 중 하나입니다.

### 대안
다른 방법으로는 CSS의 `text-transform` 속성을 사용해 웹 페이지에서 텍스트를 대문자로 바꿀 수도 있습니다. 하지만 이는 코드 측면에서 문자열을 대문자로 바꾸는 것이 아니라, 디스플레이 단에서 보이는 문자를 대문자로 바꾸는 것입니다.

### 세부 구현
`toUpperCase()`는 내부적으로 유니코드 테이블을 참조하여 작동합니다. 소문자와 대문자 간에는 일정한 차이가 있어, 이차이를 더하거나 빼서 변환을 진행합니다.

## 추천 자료

- [MDN Web Docs, String.prototype.toUpperCase()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [W3Schools, JavaScript String toUpperCase() Method](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [The Unicode Consortium](https://home.unicode.org)