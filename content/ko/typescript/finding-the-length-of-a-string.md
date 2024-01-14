---
title:    "TypeScript: 문자열의 길이 찾기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것이 중요한 이유는 프로그래밍에서 자주 사용되는 작업 중 하나이기 때문입니다. 문자열의 길이는 데이터를 처리하고 검증하는 데 필수적이며, 다양한 문제를 해결하는 데도 도움이 됩니다. TypeScript를 사용하면 문자열의 길이를 쉽게 찾을 수 있으며, 이를 통해 효율적인 코드 작성이 가능합니다.

## 어떻게

문자열의 길이를 찾는 방법은 간단합니다. TypeScript에서는 `string` 타입의 변수 뒤에 `.length`를 붙여주면 해당 문자열의 길이를 구할 수 있습니다. 예를 들어, 다음과 같은 코드를 작성합니다.

```TypeScript
let str: string = "안녕하세요!";
console.log(str.length);
```

위 코드를 실행하면 콘솔에 `7`이 출력됩니다. 이는 문자열 "안녕하세요!"가 7개의 글자로 이루어져 있기 때문입니다. 또한 TypeScript에서는 다양한 메서드를 활용하여 문자열의 길이를 조건에 따라 잘라낼 수도 있습니다.

```TypeScript
let sentence: string = "나는 오늘 너무 피곤해!";
console.log(sentence.slice(7)); // 결과: 너무 피곤해!
```

위 코드에서 `slice()` 메서드는 문자열의 일부분을 추출할 수 있습니다. 첫 번째 인자로 시작 위치를, 두 번째 인자로는 종료 위치를 지정할 수 있습니다. 만약 두 번째 인자를 생략하면, 마지막 글자까지 추출하게 됩니다.

## 깊이있는 탐구

TypeScript에서 문자열의 길이를 찾는 과정은 내부적으로 `length` 속성을 사용합니다. 이 속성은 `string` 타입이 아닌 다른 객체에도 적용할 수 있습니다. 예를 들어, `Array` 객체에도 `length` 속성이 존재하며, 배열의 길이를 구할 때 유용하게 활용됩니다.

또한 `string` 타입에서는 `charAt()` 메서드를 이용하여 특정 위치의 글자를 가져올 수 있습니다.

```TypeScript
let str: string = "Hello!";
console.log(str.charAt(0)); // 결과: H
console.log(str.charAt(3)); // 결과: l
```

이외에도 `string` 타입에서 사용할 수 있는 다양한 메서드들이 있으니, 자세히 알아보시기 바랍니다.

## 참고

- [TypeScript String length 속성](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [TypeScript String slice() 메서드](https://www.w3schools.com/jsref/jsref_slice_string.asp)
- [TypeScript String charAt() 메서드](https://www.w3schools.com/jsref/jsref_charat.asp)