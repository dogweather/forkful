---
title:    "TypeScript: 문자열을 소문자로 변환하기"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 작업에 대해 배우는 이유는 어떤 상황에서 해야 하는지 이유를 이해하기 위해서입니다.

## 방법
```TypeScript
const word = "HELLO";
console.log(word.toLowerCase());
```
`HELLO`라는 문자열을 `hello`로 변환하는 간단한 예제입니다.
```TypeScript
const sentence = "I LOVE TypeScript";
console.log(sentence.toLowerCase());
```
`I LOVE TypeScript`라는 문장을 모두 소문자로 변환하는 예제입니다.

## 깊이 파고들기
문자열을 소문자로 변환하는 작업은 대소문자를 구분하지 않는 검색 또는 비교 작업을 할 때 유용합니다. 또한, 사용자 입력에서 대문자를 허용하지 않는 경우 데이터의 일관성을 유지하는 데 도움이 됩니다. TypeScript의 `toLowerCase()` 메서드는 모든 언어의 문자열을 소문자로 변환할 수 있으며, 기본적으로 로케일에 따라 동작합니다. 이 의미는 정확한 결과를 얻기 위해서는 로케일을 설정해야 할 수도 있음을 의미합니다. 또한, `toLowerCase()` 메서드는 값을 변경하지 않고 새로운 소문자 문자열을 반환하는 불변성을 가지고 있습니다.

## 참고
* [TypeScript 문자열 메서드 공식 문서 (번역)](https://typescript-kr.github.io/pages/Basic Types - String.html#%EB%AC%B8%EC%9E%90%EC%97%B4-%EB%A9%94%EC%84%9C%EB%93%9C)