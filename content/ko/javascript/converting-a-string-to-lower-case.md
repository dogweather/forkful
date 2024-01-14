---
title:    "Javascript: 문자열을 소문자로 변환하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜

문자열을 소문자로 변환하는 이유는 다양합니다. 예를 들어 사용자의 입력을 일관된 형식으로 처리해야 하는 경우, 대소문자를 구분하지 않는 검색 기능을 구현해야 하는 경우 등이 있습니다. 따라서 문자열의 소문자 변환은 프로그래밍에서 필수적인 기능이라고 할 수 있습니다.

## 어떻게

자바스크립트에서는 `toLowerCase()` 메소드를 사용하여 문자열을 소문자로 변환할 수 있습니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Javascript 
let str = "Hello, World!";
let lowerStr = str.toLowerCase();
console.log(lowerStr); // hello, world!
```

위 코드에서 `toLowerCase()` 메소드를 호출하면 기존의 `str` 변수에 저장되어 있던 문자열이 소문자로 변환되어 `lowerStr` 변수에 저장됩니다. 즉, 변수의 값이 바뀌는 것이 아니라 새로운 문자열이 생성되는 것에 유의해야 합니다.

## 깊이 파이

`toLowerCase()` 메소드는 간단하게 사용할 수 있지만 내부적으로 어떻게 동작하는지 알아보겠습니다. 이 메소드는 문자열의 각 문자를 반복하여 대문자인 경우 해당 문자를 소문자로 변환하고, 그 외의 경우는 그대로 유지하는 방식으로 작동합니다. 따라서 대문자가 아닌 문자는 모두 그대로 유지되기 때문에 주의해야 합니다.

또한, `toLowerCase()` 메소드는 원본 문자열의 값을 바꾸는 것이 아니기 때문에 새로운 문자열이 생성되는 비용이 발생합니다. 때문에 문자열이 매우 긴 경우에는 성능 문제가 발생할 수 있으니 주의해야 합니다. 이 경우에는 다른 방법을 사용하여 문자열을 소문자로 변환하는 것이 더 효율적일 수 있습니다.

# 참고 자료

- [String.prototype.toLowerCase() MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [자바스크립트 문자열 소문자로 변환하기 | Flostudio](https://fl0.co.kr/41)
- [자바스크립트 내장 객체 스트링 관련 메서드 .toUpperCase(), .toLowerCase(), .trim() 알아보기 | leehwarang.github.io](https://leehwarang.github.io/2019/javascript-string-function/)