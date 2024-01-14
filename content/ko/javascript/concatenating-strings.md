---
title:                "Javascript: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

스트링을 결합하는 것을 왜 해야 할까요? 이것은 프로그래밍에서 매우 일반적인 작업입니다. 알아봅시다!

## 어떻게

스트링을 결합하는 것은 매우 간단합니다. 우리는 `+` 기호를 사용하여 스트링을 하나씩 추가하기만 하면 됩니다. 예를 들어:

```javascript
let firstName = "박";
let lastName = "지연";
let fullName = firstName + lastName;
console.log(fullName);
```

결과는 "박지연"이 될 것입니다.

여러 개의 스트링을 결합할 수도 있습니다. 예를 들어:

```javascript
let firstPart = "안녕";
let secondPart = "하세요";
let thirdPart = "!";
let greeting = firstPart + secondPart + thirdPart;
console.log(greeting);
```

출력은 "안녕하세요!"가 될 것입니다.

## 딥 다이브

스트링을 결합하는 것은 문자열 연산의 일부입니다. 문자열 연산은 문자열을 조작하거나 변환할 때 사용됩니다. 예를 들어, `+` 기호를 사용하여 스트링 결합 외에도, 그 외에도 문자열 길이를 알 수 있는 `length` 속성이 있습니다. 그리고 문자열의 일부를 추출하는 `substring()` 메소드 등 여러 가지 작업을 할 수 있습니다.

## 또 보기

* [JavaScript 문자열 연산 가이드](https://www.w3schools.com/js/js_string_methods.asp)
* [JavaScript에서 스트링 결합하기](https://www.javatpoint.com/javascript-string-concat)
* [스트링 연산에 대한 MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)