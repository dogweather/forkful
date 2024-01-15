---
title:                "문자열 대문자로 변환하기"
html_title:           "TypeScript: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
문자열을 대문자로 바꾸는 작업을 수행하는 이유를 설명합니다.

예를 들어, 사용자가 입력한 문자열을 데이터베이스에 저장하거나, 외부 API를 사용할 때 대문자로 변환하여 일관성을 유지하거나, 일부 데이터의 비교 및 검색이 용이하도록 하기 위해 문자열을 대문자로 바꿀 수 있습니다.

## 사용 방법
아래의 예제 코드 블록을 참고하여 TypeScript를 사용하여 문자열을 대문자로 바꾸는 방법을 배우세요.

```TypeScript
// 입력 받은 문자열
let str = "hello world";

// 문자열을 대문자로 변환
let capitalizedStr = str.toUpperCase();

// 변환된 문자열 출력
console.log(capitalizedStr);

// Output: HELLO WORLD
```

```TypeScript
// 변수에 할당된 문자열 대문자로 변환
let str = "happy birthday";

// 기존 변수에 대문자로 변환된 문자열 덮어쓰기
str = str.toUpperCase();

// 변환된 문자열 출력
console.log(str);

// Output: HAPPY BIRTHDAY
```

## 더 깊게
문자열을 대문자로 바꾸는 작업은 일반적으로 문자열 처리에서 자주 사용됩니다. TypeScript에서는 `toUpperCase()` 메서드를 사용하여 간단하게 대문자로 바꿀 수 있습니다. 

또한, `toLocaleUpperCase()` 메서드를 사용하여 현재 사용자의 로케일에 맞는 대문자로 변환할 수도 있습니다. 이를 통해 문자열 처리에 더 많은 유연성을 추가할 수 있습니다.

## 참고 자료
- [TypeScript: 문자열 메서드](https://www.typescriptlang.org/docs/handbook/strings.html)
- [MDN: toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN: toLocaleUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)

## 참고자료
- [TypeScript: 문자열 메서드 (https://www.typescriptlang.org/docs/handbook/strings.html)
- [MDN: toUpperCase() (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN: toLocaleUpperCase() (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)