---
title:                "문자열의 대문자화"
html_title:           "TypeScript: 문자열의 대문자화"
simple_title:         "문자열의 대문자화"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열의 첫 번째 글자를 대문자로 변환하는 작업을 캐피탈라이즈(capitalize)라고 합니다. 프로그래머들이 이 작업을 하는 이유는 사용자에게 보기 좋은 텍스트를 제공하기 위해서입니다.

## 어떻게:

```TypeScript
const str = "hello world";
console.log(str.charAt(0).toUpperCase() + str.slice(1));

//Output: Hello world
```

```TypeScript
const str = "typescript programming";
console.log(str.split(" ").map(word => word.charAt(0).toUpperCase() + word.slice(1)).join(" "));

//Output: TypeScript Programming
```

## 딥 다이브:

(1) 캐피탈라이즈 작업은 대문자나 소문자와 같은 문자열 변환 작업의 일부입니다. (2) 다른 언어에서는 capitalize() 함수를 제공하지만 TypeScript에서는 위 코드와 같이 수동으로 작업해야 합니다. (3) 또한 위 코드에서는 첫 번째 글자만 대문자로 변환하지만 다른 구현 방식으로 모든 단어의 첫 번째 글자를 대문자로 변환할 수도 있습니다.

## 관련 자료:

- [JavaScript 문자열 변환 함수](https://www.w3schools.com/js/js_string_methods.asp)
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/)