---
title:                "문자열 연결하기"
html_title:           "TypeScript: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
문자열을 연결하는 것은 우리가 문자열을 일련의 부분으로 나누어 작업하는 것보다 컴퓨터 프로그램에서 더욱 유용한 방법입니다. 프로그래머는 여러 개의 문자열을 한 번에 하나의 문자열로 합쳐야 할 때가 종종 있기 때문에 이 작업을 자주합니다.

## 하는 방법:
```TypeScript
let string1 = "안녕하세요";
let string2 = "반가워요";

let combinedString = string1 + string2;

console.log(combinedString);
```
```
**출력:** 안녕하세요반가워요
```

## 깊이 파헤치기:
1. 역사적 맥락: 문자열을 연결하는 개념은 프로그래밍 언어에 따라 조금 다르지만, 기본적으로 문자열을 조작하는 기본적인 작업 중 하나입니다. 이러한 작업은 오래된 컴퓨터 프로그래밍에서부터 사용되어 왔습니다.

2. 대안: 문자열을 연결하는 대신에 문자열 보간(interpolation)을 하는 방법도 있습니다. 이는 보다 편리한 방법으로 문자열을 조작하는데 사용될 수 있습니다.

3. 구현 세부사항: 문자열 연결은 큰 문자열을 생성하는 작업에는 비용이 많이 드는 작업일 수 있으므로, 문자열 연결의 경우 문자열 보간을 대신 사용하는 것이 더 좋을 수 있습니다.

## 관련 정보:
- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-interpolation)