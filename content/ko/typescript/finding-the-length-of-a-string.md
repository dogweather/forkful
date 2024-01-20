---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그래야하는가? 
문자열의 길이를 찾는 것은, 문자열이 얼마나 많은 문자를 포함하고 있는지 계산하는 것입니다. 프로그래머들이 이 작업을 수행하는 이유는, 입력 값을 검증하거나, 문자열 조작을 할 때 기본적인 시작점이 필요하기 때문입니다.

## 방법:

```TypeScript
let str: string = "안녕하세요, TypeScript";
console.log(`문자열의 길이 : ${str.length}`);
```

이 코드를 실행하면 다음과 같은 결과가 나타납니다.

```Shell
문자열의 길이 : 17
```

## 깊게 알아보기 :
문자열의 길이를 찾는 방법은 프로그래밍의 기본 중 하나이며, 코드 최적화의 중요한 요소입니다. 과거에는 루프를 이용해 문자를 하나씩 세어 문자열의 길이를 찾았지만, 현대의 대부분의 프로그래밍 언어에서는 내장메서드를 제공하여 이 작업을 단순화하였습니다.

뿐만 아니라, TypeScript는 JavaScript를 확장한 언어이므로, JavaScript의 `length` 속성을 그대로 사용할 수 있습니다.

참고로, 문자열의 `length` 속성은 Unicode 코드 포인트의 개수를 반환하는 것이다. 그래서 이모티콘이나 특수한 문자를 처리할 때는 주의가 필요하다.

## 참조 
- MDN Web Docs의 "String.length" (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- TypeScript 공식 문서 (https://www.typescriptlang.org/docs/handbook/basic-types.html#string)