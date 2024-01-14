---
title:    "TypeScript: 대소문자 변환하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜: 문자열을 소문자로 변환할 때 이점

문자열을 소문자로 변환하는 것은 많은 이유로 유용합니다. 일반적으로, 우리는 문자열을 비교하거나 정렬할 때 대소문자를 고려하게 됩니다. 따라서, 문자열을 모두 소문자로 변환하면 비교나 정렬이 더 쉬워지게 됩니다. 또한, 사용자가 입력한 문자열을 소문자로 변환하여 데이터를 일관성 있게 관리할 수 있습니다. 

## 방법: TypeScript로 문자열을 소문자로 변환하는 법

우선, 문자열을 소문자로 변환하기 위해서는 `toLowerCase()` 메소드를 사용해야 합니다. 이 메소드는 문자열을 소문자로 변환한 새로운 문자열을 반환합니다. 예시를 살펴보겠습니다.

```typescript
const text = "HELLO WORLD";
const lowercase = text.toLowerCase();
console.log(lowercase); // 출력 결과: hello world
```

위의 예시에서는 `toLowerCase()` 메소드를 사용하여 `text` 변수에 할당된 문자열을 소문자로 변환한 뒤, `console.log()`를 통해 출력했습니다. 이와 같이 `toLowerCase()` 메소드를 사용하면 쉽게 문자열을 소문자로 변환할 수 있습니다.

## 깊게 들어가보기: 문자열을 소문자로 변환하는 더 자세한 정보

`toLowerCase()` 메소드는 내부적으로 유니코드를 사용해 문자열을 소문자로 변환합니다. 이때, 유니코드 표준에 따라 영어 알파벳 외의 문자도 변환될 수 있습니다. 예를 들어, "İ"와 같은 문자는 소문자로 변환하면 "i"가 아니라 "ı"로 변환됩니다. 따라서 유니코드 표준에 따라 문자열을 소문자로 변환했을 때 예상하지 못한 결과가 나타날 수 있으므로 주의가 필요합니다.

또한, `toLowerCase()` 메소드는 문자열을 변환한 새로운 문자열을 반환하므로 기존의 문자열이 변경되지 않습니다. 이를 유의해서 적절하게 사용해야 합니다.

## 참고: 관련 링크들

- [JavaScript String toLowerCase() 메소드](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [TypeScript 문자열 관련 메소드](https://www.typescriptlang.org/docs/handbook/2/strings.html)
- [유니코드 표준](https://unicode.org/)