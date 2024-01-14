---
title:    "TypeScript: 문자열 대문자로 변환하기"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것에 대해 1-2 문장으로 설명합니다.

## 하는 방법

문자열을 대문자로 바꾸는 방법에 대한 코딩 예제와 "```TypeScript ... ```" 코드 블록 내에서의 샘플 출력을 제공합니다. 

```TypeScript
let str = "hello world";
console.log(str.toUpperCase()); // "HELLO WORLD"
```

### 더 깊이 들어가보기

문자열을 대문자로 바꾸는 방법에 대해 더 깊이 알아봅니다. 우리는 `toUpperCase()` 메소드를 통해 문자열을 손쉽게 대문자로 변환할 수 있지만, `toUpperCase()` 메소드는 영어 알파벳에만 적용되기 때문에 다른 언어의 문자를 대문자로 변환하기 위해서는 추가적인 처리가 필요합니다. 또한, 대소문자를 구별하지 않는 문자열에서 대문자로 변환하거나, 특정 지점부터 대문자로 바꾸는 등의 다양한 변환 방법을 살펴보고 실제로 사용할 수 있는 예제를 제공합니다.

## 관련 자료

"## 관련 자료" 제목으로 마크다운 헤딩을 추가하고, 아래에 관련된 링크를 나열합니다. 

- [자바스크립트에서 대소문자 변환하기](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)
- [유니코드 대소문자 변환](https://unicode.org/charts/case/)