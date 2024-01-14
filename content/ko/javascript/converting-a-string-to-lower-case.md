---
title:                "Javascript: 문자열 소문자로 변환하기"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 이유는 프로그래밍에서 매우 일반적인 작업 중 하나입니다. 대소문자에 따라 코드의 결과가 달라질 수 있기 때문에, 문자열을 일관된 형식으로 표시하고 비교하는 데 중요합니다.

## 방법

```Javascript
let string = "Hello World!";
let lowerCase = string.toLowerCase();

console.log(lowerCase); // 출력 결과: hello world!
```

이 간단한 예제에서, `toLowerCase()` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 이 함수는 문자열의 모든 대문자를 소문자로 변환해줍니다.

각 언어마다 문자열을 소문자로 변환하는 방법은 조금씩 다를 수 있지만, 대부분의 언어에서 유사한 함수를 제공합니다. 따라서 자바스크립트에서 이 함수를 사용하는 방법을 배우면, 다른 언어에서도 쉽게 적용할 수 있습니다.

## 깊게 파헤치기

문자열을 소문자로 변환하는 것은 간단해 보이지만, 그 안에는 깊은 이해가 필요합니다. 예를 들어, 특정 언어의 대소문자 규칙을 고려해야 할 수도 있고, 변환된 문자열이 원래의 문자열과 정확히 일치하는지 확인해야 할 수도 있습니다.

또한, 자바스크립트에서는 문자열뿐만 아니라 배열이나 객체 내의 문자열도 소문자로 변환할 수 있습니다. 이 경우에는 `map()` 메서드와 같은 다른 함수를 함께 사용해야 합니다. 이러한 세부 사항을 이해하면, 보다 유연하고 강력한 코드를 작성할 수 있습니다.

## 또 다른 방법 알아보기

* [자바스크립트 공식 문서 - String.prototype.toLowerCase()](https://developer.mozilla.org/ko/docs/Web/JavaScrip