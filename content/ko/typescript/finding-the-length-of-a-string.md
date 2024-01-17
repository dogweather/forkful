---
title:                "문자열의 길이 찾기"
html_title:           "TypeScript: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

먼저, 문자열의 길이를 구하는 것은 문자열의 길이를 알아내는 것입니다. 프로그래머는 이를 하는 이유는 다양합니다. 예를 들어, 문자열의 길이를 알아내면 입력한 데이터가 적절한지, 표시해야 할 문자의 수를 조정해야 할지 등을 판단할 수 있습니다.

## 방법:

**TypeScript**를 사용하여 문자열의 길이를 구하는 방법은 아주 간단합니다. ```string``` 데이터 타입을 사용하고, ```.length``` 프로퍼티를 이용하여 문자열의 길이를 구할 수 있습니다. 아래 코드를 참고해 보세요.

```typescript
let name: string = "Jane";

console.log(name.length); // 결과: 4
```

## 깊이 탐구:

1. **역사적 배경:** 문자열의 길이를 구하는 방법은 오래 전부터 존재했습니다. 그럼에도 불구하고, 이는 프로그래머들에게 매우 중요한 작업이며, 오늘날 알고리즘과 프로그래밍 언어가 발전함에 따라 더욱 쉽고 효율적으로 구현할 수 있게 되었습니다.

2. **대안:** 문자열의 길이를 구하는 다른 방법으로는 ```string``` 데이터 타입을 배열로 변환한 뒤, 배열의 길이를 구하는 방법이 있습니다.

3. **구현 세부 사항:** 문자열의 길이를 알아내는 알고리즘은 문자 하나씩 반복하며 카운트하는 방법을 사용합니다. 이를 코드로 나타내면 아래와 같습니다.

```typescript
function getStringLength(str: string): number {
  let count: number = 0;

  for (let i = 0; i < str.length; i++) {
    count++;
  }

  return count;
}
```

## 관련자료:

1. [TypeScript 공식 사이트](https://www.typescriptlang.org/)
2. [JavaScript로 문자열의 길이 구하기](https://www.w3schools.com/jsref/prop_str_length.asp)
3. [Codecademy - TypeScript 배우기](https://www.codecademy.com/learn/learn-typescript)