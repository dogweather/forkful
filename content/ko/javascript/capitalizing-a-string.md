---
title:                "Javascript: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜?

문자열의 첫 글자를 대문자로 바꾸는 것은 많은 프로그래밍 언어에서 자주 사용되는 작업 중 하나입니다. 그 이유는 보통 문장의 제목을 화려하게 표현하고자 할 때, 또는 사용자 입력값을 정리하거나 검사할 때, 그리고 많은 다른 이유로 인해 문자열의 첫 글자를 대문자로 바꾸는 경우가 많기 때문입니다.

## 방법

문자열의 첫 글자를 대문자로 바꾸기 위해서는 `toUpperCase()` 메소드를 사용하면 됩니다. 아래는 자바스크립트에서 문자열을 대문자로 바꾸는 간단한 예제입니다.

```Javascript
let str = "hello world";
let capStr = str[0].toUpperCase() + str.slice(1);
console.log(capStr); // "Hello world"
```

위의 예제에서 `toUpperCase()` 메소드는 해당 문자열의 첫 번째 글자를 대문자로 변환하고, `slice()` 메소드를 사용하여 첫 글자를 제외한 나머지 부분을 가져옵니다. 이렇게 하면 문자열의 첫 글자만 대문자로 변환하여 출력할 수 있습니다.

또는 정규 표현식을 사용하여 문자열의 첫 글자를 대문자로 바꿀 수도 있습니다. 아래의 예제는 `replace()` 메소드와 정규 표현식을 활용한 방법입니다.

```Javascript
let str = "javascript is fun";
let capStr = str.replace(/^\w/, (c) => c.toUpperCase());
console.log(capStr); // "Javascript is fun"
```

## 깊게 들어가기

문자열을 대문자로 바꾸는 것은 간단한 작업처럼 보이지만, 실제로는 내부적으로 많은 과정이 있습니다. 자바스크립트에서는 문자열이 내부적으로는 배열과 비슷한 형태로 저장됩니다. 따라서 `toUpperCase()` 메소드가 동작하는 방식도 배열에 적용되는 메소드와 유사합니다. 또한, 정규 표현식을 사용하는 방법 또한 문자열이나 배열을 처리하는 데에 유용하게 쓰일 수 있습니다.

또한, 자바스크립트에서는 객체지향 프로그래밍 언어로서의 특징을 갖기 때문에, 문자열을 대문자로 변환하는 작업 역시 객체의 속성이나 메소드를 활용하여 구현할 수 있습니다.

## 더 알아보기

위에서 소개한 방법 이외에도 여러 가지 방법으로 문자열의 첫 글자를 대문자로 바꿀 수 있습니다. 관련된 다양한 정보를 더 알아보시려면 아래의 링크들을 참고해보세요.

[MDN - String.prototype.toUpperCase()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)

[MDN - String.prototype.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)

[MDN - Regular Expressions](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)

[MDN - 객체지향 프로그래밍](https://developer.mozilla.org/ko/docs/Learn/JavaScript/Objects/Object-oriented_JS)