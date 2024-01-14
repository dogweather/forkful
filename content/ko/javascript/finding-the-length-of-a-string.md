---
title:    "Javascript: 문자열의 길이 찾기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 것은 프로그래밍에서 매우 중요한 기능입니다. 이 기능은 문자열을 다루는 많은 작업에 유용하기 때문에 항상 사용됩니다.

# 방법

우리는 자바스크립트에서 문자열의 길이를 찾는 방법에 대해 알아보겠습니다. 우선, 문자열의 길이를 찾는 가장 간단한 방법은 `length` 속성을 사용하는 것입니다. 예를 들어서, "Hello"라는 문자열의 길이는 `5`가 됩니다. 아래는 코드 예시와 그에 대한 출력입니다.

```Javascript
// 코드 예시
let str = "Hello";
console.log(str.length);
// 출력: 5
```

또 다른 방법은 `split()` 메소드를 사용하여 공백을 기준으로 문자열을 배열로 변환하고, 그 길이를 구하는 것입니다. 아래는 다른 예시와 출력입니다.

```Javascript
// 코드 예시
let str = "Hello World";
console.log(str.split(" ").length);
// 출력: 2
```

# 깊이 파고들기

우리는 이제 문자열의 길이를 찾는 방법들을 살펴보았습니다. 그러나 실제로 자바스크립트에서 문자열의 길이를 어떻게 구하는지에 대해 더 깊이 파고들어보겠습니다.

자바스크립트에서는 문자열의 각 문자들이 내장된 `String` 객체의 `length` 속성을 가지고 있습니다. 따라서 `String` 객체의 `length` 값을 호출하면 해당 문자열의 길이를 얻을 수 있습니다.

또한 `split()` 메소드를 사용하여 공백을 기준으로 문자열을 배열로 변환할 때, 해당 배열의 길이를 구하면 공백을 포함한 단어의 수를 얻을 수 있습니다.

# 관련 링크

- [MDN | String.length](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN | String.prototype.split()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/split)