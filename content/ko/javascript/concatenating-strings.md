---
title:                "문자열 연결"
html_title:           "Javascript: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것의 가장 큰 이유는 다양한 문자열을 단일 문자열로 조합하여 더 긴 문자열을 생성하는 것입니다. 이를 통해 데이터의 유연성을 향상시키고 보다 효율적인 코드 작성이 가능하며, 다양한 문제에 대한 해결책을 찾기 위한 방법으로 활용할 수 있습니다.

## 어떻게

먼저, 우리는 기본적인 문자열 연결 방법을 알아보겠습니다. 다음은 "Hello"와 "World"를 연결하여 "Hello World"를 출력하는 간단한 예제 코드입니다.

```Javascript
const greeting = "Hello";
const name = "World";

console.log(greeting + " " + name);
// Output: Hello World
```

또한 우리는 빽틱(backticks) 기호를 활용하여 더 간략하고 직관적인 방법으로 문자열을 연결할 수 있습니다.

```Javascript
const greeting = "Hello";
const name = "World";

console.log(`${greeting} ${name}`);
// Output: Hello World
```

또 다른 방법으로, 문자열을 담고 있는 배열을 이용하여 다수의 문자열을 연결할 수 있습니다.

```Javascript
const languages = ["Javascript", "Python", "Java"];

const output = "My favorite programming languages are " + languages.join(", ");

console.log(output);
// Output: My favorite programming languages are Javascript, Python, Java
```

## 딥 다이브

자바스크립트에서 문자열을 연결하는 방법은 다양한 내장 함수들을 통해 보다 유연하게 활용할 수 있습니다. 예를 들어, `concat()` 함수를 이용하여 여러 개의 문자열을 연결할 수 있으며, `slice()` 함수를 통해 특정 부분의 문자열만을 추출하여 연결하는 것도 가능합니다.

또한, 빽틱 기호를 활용하는 것 외에도 ES6에서 추가된 템플릿 리터럴(template literal)을 이용해 문자열을 보다 쉽고 간결하게 연결할 수 있으며, 문자열 뿐만 아니라 변수나 함수 등을 함께 사용할 수도 있습니다.

## 또 다른 예제

아래는 템플릿 리터럴을 이용한 예제 코드입니다.

```Javascript
const name = "Willy";
const age = 25;

console.log(`My name is ${name} and I am ${age} years old.`);
// Output: My name is Willy and I am 25 years old.
```

## 참고 자료

다음은 문자열 연결에 대해 더 자세한 내용을 알고 싶은 경우 참고할 수 있는 자료들입니다.

- [MDN | String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [The Modern Javascript Tutorial | Strings](https://javascript.info/string)
- [W3Schools | Javascript String concatenation](https://www.w3schools.com/js/js_string_concat.asp)