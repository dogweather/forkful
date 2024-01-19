---
title:                "문자열 보간하기"
html_title:           "Clojure: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 보간이란 변수, 결과 또는 식을 문자열 내에 쉽게 삽입하는 프로그래밍 기법입니다. 이것은 코드의 가독성을 개선하고 동적인 내용을 수동으로 조합하는 번거로움을 줄여주세요.

## 방법:

아래는 JavaScript에서 문자열 보간을 사용하는 방법을 보여주는 예입니다.

```Javascript
let name = "지민";
let intro = `안녕하세요, ${name}입니다.`;
console.log(intro); // "안녕하세요, 지민입니다."
```
#
이러한 활용은 동적으로 변하는 값을 쉽게 문자열에 삽입할 수 있게 만들어줍니다.

```Javascript
let apples = 5;
let oranges = 7;
let fruitSummary = `나는 사과 ${apples}개와 오렌지 ${oranges}개를 가지고 있습니다.`;
console.log(fruitSummary); // "나는 사과 5개와 오렌지 7개를 가지고 있습니다."
```

## 깊은 분석:

**역사적 배경**: 이전에는 문자열 안에 변수를 삽입하려면 '+' 연산자를 사용하는 복잡한 프로세스가 필요했습니다. ES6 (ECMAScript 2015)에서 템플릿 리터럴 및 문자열 보간이 도입되면서 개발자들은 문자열 생성에 더욱 편리한 방법을 갖게 되었습니다.

**대체 방법**: 문자열 연결은 문자열 보간의 주요 대체 방법입니다. 하지만, 이 방법은 가독성이 낮고 코드가 더 길어지므로 피하는 것이 좋습니다.

```Javascript
let name = "지민";
let intro = "안녕하세요, " + name + "입니다.";
console.log(intro); // "안녕하세요, 지민입니다."
```

**구현 세부 사항**: 문자열 보간은 백틱(`)을 사용하여 감싸진 템플릿 리터럴과 ${}를 사용하여 식을 삽입함으로써 작동합니다.

## 참고 자료:

1. [MDN Web Docs - 템플릿 리터럴](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)
2. [JavaScript.info - 문자열](https://ko.javascript.info/string)
3. [W3Schools - JavaScript String Interpolation](https://www.w3schools.com/js/js_string_templates.asp)