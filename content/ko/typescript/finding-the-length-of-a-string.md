---
title:    "TypeScript: 문자열의 길이 찾기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜 

문자열의 길이를 찾는 것에 대해 왜 관심을 가지게 되는지에 대해서는 여러 가지 이유가 있습니다. 예를 들어, 프로그래밍에서는 문자열을 다루는 경우가 많기 때문에 문자열의 길이를 알아야 할 필요가 있습니다. 또한, 문자열의 길이를 알면 얼마나 많은 메모리를 할당해야 하는지를 결정하는 데에도 도움이 됩니다. 따라서 문자열의 길이를 알아내는 것은 프로그래밍에서 기본적이고 중요한 개념입니다.

## 방법

TypeScript를 사용하여 문자열의 길이를 찾는 방법은 다양합니다. 가장 간단한 방법은 내장된 `length` 속성을 사용하는 것입니다. 예를 들어, 다음과 같이 문자열 변수 `str`에 있는 문자열의 길이를 찾을 수 있습니다.

```TypeScript
let str = "안녕하세요";
console.log(str.length);
```

위 코드의 결과는 `5`가 출력됩니다. 이는 문자열 `str`에 5개의 문자가 포함되어 있기 때문입니다. 

또 다른 방법은 `string` 타입의 메소드인 `length()`를 사용하는 것입니다. 이 메소드는 주어진 문자열의 실제 길이를 반환해줍니다. 아래의 예시 코드를 참고하시기 바랍니다.

```TypeScript
let str = "반가워!";
console.log(str.length());
```

위 코드의 결과는 `3`이 됩니다. 이는 문자열 `str`에 실제로 `3`개의 글자가 있기 때문입니다. 

TypeScript를 사용하여 문자열의 길이를 찾는 또 다른 방법은 `String` 함수를 사용하는 것입니다. 이 함수는 주어진 문자열의 길이를 반환해줍니다. 예를 들어, 다음과 같이 `String` 함수를 사용하여 문자열의 길이를 찾을 수 있습니다.

```TypeScript
let str = "Good morning";
console.log(String(str).length);
```

위 코드의 결과는 `12`가 됩니다. 이는 문자열 `str`에 있는 문자의 개수가 `12`개이기 때문입니다.

## 깊이 파고들기

실제로 문자열의 길이를 찾기 위해 컴퓨터는 어떻게 동작할까요? 간단히 말하면, 컴퓨터는 주어진 문자열의 맨 끝에 있는 특수한 문자를 찾습니다. 이러한 문자를 널(null) 문자라고 합니다. 그리고 널 문자가 나타나기까지 문자열의 길이를 세어서 반환합니다. 

널 문자의 역할은 문자열의 끝을 알려주는 것입니다. 따라서 컴퓨터는 널 문자를 찾기까지 문자열을 계속 읽어가며 길이를 카운트합니다. 그리고 널 문자를 찾으면 계산한 길이를 반환합니다.

## 연관 정보

- [TypeScript 공식 문서](https://www.typescriptlang.org/)
- [JavaScript 문자열 길이 찾는 방법](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [String 함수에 대한 자세한 설명(영어)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)