---
title:                "TypeScript: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트에서 정규 표현식을 사용하는 이유는 문자열에서 패턴을 찾거나 대체, 분리할 수 있기 때문입니다.

## 사용 방법

정규 표현식을 사용하는 방법은 간단합니다. 먼저, `RegExp` 객체를 사용하여 패턴을 만들어야 합니다. 그 다음 `test()` 메소드를 사용하여 해당 패턴이 문자열에 존재하는지 확인할 수 있습니다. 만약 문자열에서 패턴을 찾는다면 `true`를 반환하고, 찾지 못한다면 `false`를 반환합니다.

```TypeScript
const regex = /hello/;

// 문자열에서 패턴이 존재하는지 확인
console.log(regex.test("hello world")); // 결과: true
console.log(regex.test("goodbye world")); // 결과: false
```

또한, `match()` 메소드를 사용하여 문자열에서 패턴에 맞는 부분을 찾아 배열로 반환할 수 있습니다.

```TypeScript
const regex = /hello/;

// 문자열에서 패턴에 맞는 부분 찾기
console.log("hello world".match(regex)); // 결과: ["hello"]
console.log("goodbye world".match(regex)); // 결과: null
```

정규 표현식은 패턴을 나타내는 문자열이기 때문에 변수를 사용할 수도 있습니다. `RegExp()` 생성자 함수를 사용하여 동적으로 패턴을 만들 수 있습니다.

```TypeScript
const name = "John";
const regex = new RegExp(`Hello, ${name}!`);

console.log(regex.test("Hello, John!")); // 결과: true
console.log(regex.test("Hello, Jane!")); // 결과: false
```

## 깊이 파고들기

정규 표현식에는 다양한 메타 문자와 기능이 있습니다. 간단한 예제로는 `^`를 사용하여 문자열의 시작을 나타낼 수 있고, `$`를 사용하여 문자열의 끝을 나타낼 수 있습니다.

```TypeScript
const regex = /^Hello/; // 문자열의 시작이 "Hello"로 시작하는지 확인하는 패턴

console.log(regex.test("Hello, world!")); // 결과: true
console.log(regex.test("Goodbye, world!")); // 결과: false
```

또한 `()`를 사용하여 그룹을 만들 수 있고, `|`를 사용하여 여러 패턴 중 하나를 찾을 수 있습니다.

```TypeScript
const regex = /(hello|goodbye) world/; // "hello world" 또는 "goodbye world" 패턴을 찾는 그룹

console.log("hello world".match(regex)); // 결과: ["hello world"]
console.log("goodbye world".match(regex)); // 결과: ["goodbye world"]
```

더 자세한 정보는 [MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/RegExp)를 참고하세요.

## 관련 정보

[TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)에서도 정규 표현식에 대해 더 많은 정보를 확인할 수 있습니다. 또한 [자바스크립트에 대한 정규 표현식 강좌](https://regexone.com/)를 통해 연습해보세요.