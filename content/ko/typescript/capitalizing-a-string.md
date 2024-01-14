---
title:                "TypeScript: 문자열 대문자로 바꾸기"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 번째 문자를 대문자로 바꾸는 것이 중요한 이유는 예를 들어 사용자가 입력한 이름을 제대로 표시하기 위해서입니다.

## 하는 방법

```TypeScript
function capitalizeString(str: string): string {
    if (str.length === 0) {
        return "";
    }
    let firstChar = str[0].toUpperCase(); // 첫 번째 문자를 대문자로 바꿈
    let restOfStr = str.slice(1); // 첫 번째 문자 이후의 문자열을 자름
    return firstChar + restOfStr; // 첫 번째 문자와 나머지 문자열을 합쳐 리턴
}

let name: string = "john"; // 이름을 입력
console.log(capitalizeString(name)); // "John" 출력
```

## 깊게 들어가기

본격적으로 문자열을 대문자로 바꾸는 방법에 대해 알아보겠습니다. 위에서 사용된 함수 `toUpperCase()`는 문자열의 첫 번째 문자를 대문자로 바꾸는 내장 함수입니다. 이 함수를 사용하면 손쉽게 첫 번째 문자를 대문자로 바꿀 수 있습니다. 또한 `slice()` 함수를 사용하면 문자열을 자를 수 있습니다. 위 예제에서는 첫 번째 문자를 제외한 나머지 문자열을 자르고, `toUpperCase()`로 바뀐 첫 번째 문자와 합쳐서 리턴하였습니다. 이러한 과정을 통해 문자열의 첫 번째 문자열을 대문자로 바꿀 수 있습니다.

## 더 알아보기

자바스크립트의 문자열은 불변(immutable)이기 때문에 문자열에 직접적으로 접근하여 수정할 수 없습니다. 따라서 위와 같이 새로운 문자열을 생성하여 필요한 변형을 가한 후 리턴해야 합니다. 문자열을 대문자로 변환하는 새로운 기능을 만들고 싶다면, 위 코드를 활용하여 만들 수 있습니다. 또한 입력으로 받은 문자열의 길이가 0일 때 예외처리를 해주어야 합니다. 이와 같은 세부적인 사항들을 고려하여 보다 완벽한 함수를 작성할 수 있습니다.

## 더 알아보기

- MDN Web Docs: [toUpperCase()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- MDN Web Docs: [slice()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/slice)

## 함께 보기

- MDN Web Docs: [문자열 다루기](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)