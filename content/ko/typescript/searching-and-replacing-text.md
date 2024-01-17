---
title:                "텍스트 검색 및 교체"
html_title:           "TypeScript: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

텍스트 검색 및 대체는 프로그래머들이 코드 내에서 원하는 내용을 찾고 변경하기 위해 사용하는 기술입니다. 이를 통해 코드의 일관성을 유지하고 버그를 수정하거나 원하는 결과를 얻을 수 있습니다.

## 하는 방법:

```TypeScript
// 텍스트 검색 및 대체 예시

// 대상 텍스트에서 'Hello'를 'Hola'로 대체하고 변수에 저장
let newText = "Hello World".replace("Hello", "Hola");

console.log(newText); // output: Hola World

// 정규식을 사용하여 모든 숫자를 '#'으로 대체
let text = "123abc456def789";
let replacedText = text.replace(/[0-9]/g, "#");

console.log(replacedText); // output: ###abc###def###

```

## 더 깊게:

텍스트 검색 및 대체는 프로그래밍에 필수적인 기술이며 오래된 기술입니다. 예전에는 이를 수동으로 수행하거나 복잡한 정규식을 사용하여 수행했지만, 현재는 많은 언어에서 내장 함수로 지원하고 있습니다. 유사한 기능으로는 코드의 일부분만 변경하는 '리팩토링'이 있습니다. 텍스트 검색 및 대체는 프로그래머의 생산성을 높이는데 큰 도움이 됩니다.

## 관련 자료:
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-examples)
- [마이크로소프트 데브 블로그의 텍스트 검색 및 대체 사용법](https://devblogs.microsoft.com/typescript/announcing-typescript-3-9/#replace-string-with-regexp)