---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 문자열을 소문자로 변환하기: TypeScript를 활용하다

## 무엇 & 왜?

문자열을 소문자로 변환하는 것은 큰 문자를 작은 문자로 바꾸는 것을 의미합니다. 프로그래머들이 이를 수행하는 이유는, 대/소문자 구분 없이 문자열 비교를 하거나, 사용자 입력을 표준화하기 위해서입니다.

## 어떻게 하는가:

```TypeScript
let str: string = "Hello World!";
let lowerCaseStr: string = str.toLowerCase();
console.log(lowerCaseStr);  // "hello world!"
```

위 코드는 문자열 "Hello World!" 를 소문자로 변환하여 "hello world!"를 단순히 출력합니다.

## 심화 학습

이 메소드는 JavaScript가 최초로 출시된 이후, 포함된 기능 중 하나였습니다. 이 기능은 사용이 간단하고 직관적이며 그리하여 많은 개발자들이 이용합니다. 그렇지만, 문자열을 소문자로 변환하는 다양한 방법이 있습니다. 

예를 들어, 배열 메소드와 함께 ES6의 화살표 함수를 사용할 수도 있습니다:

```TypeScript
let str = 'Hello World!';
let lowerCaseStr = [...str].map(ch => ch.toLowerCase()).join('');
```

또한, 문자열의 대소문자를 변환하는 다른 언어와 라이브러리에서 제공하는 특별한 메소드도 있습니다. 예를 들면, Python에는 `lower()`이라는 메소드가 있고, Java에는 `toLowerCase()` 메소드가 있습니다. 

## 참고 자료:

다음은 배울 수 있는 관련 자료들 입니다:

1. [MDN Web Docs - String.prototype.toLowerCase()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
2. [TypeScript - Working with Strings](https://www.typescripttutorial.net/typescript-tutorial/typescript-strings/)