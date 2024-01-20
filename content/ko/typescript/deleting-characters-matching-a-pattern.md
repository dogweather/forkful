---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 그래요?

패턴에 일치하는 문자를 삭제하는 것은 문자열에서 특정 패턴에 일치하는 모든 문자를 제거하는 프로그래밍 작업입니다. 이는 불필요한 문자나 특수 문자를 제거하거나, 문자열 정규화 등의 작업을 수행하는 데 필요합니다.

## 어떻게 하는가:

다음은 TypeScript에서 문자열 내 패턴에 일치하는 모든 문자를 제거하는 방법에 대한 코드 예제입니다.

```TypeScript
let str = "안녕하세요, 파이썬!";
let pattern = /[^가-힣 ]/g;
str = str.replace(pattern, "");
console.log(str); // 출력: "안녕하세요 파이썬"
```

이 코드는 `[^가-힣 ]` 패턴에 일치하는 모든 문자를 찾아 제거합니다. 이 패턴은 한글과 공백을 제외한 모든 문자에 일치합니다.

## 깊은 이해:
문자열에서 패턴에 일치하는 문자를 삭제하는 작업은 언어의 차이에 불구하고 본질적으로 동일합니다. 과거에는 패턴 일치를 위해 복잡한 알고리즘을 사용했지만, 현재는 대부분의 언어가 기본적으로 정규 표현식을 지원합니다.

비록 TypeScript가 JavaScript로 컴파일돼 실행되는 언어임에도 불구하고, TypeScript에서는 문자열 메서드와 정규 표현식을 이용해 이 작업을 쉽게 수행할 수 있습니다.

대안으로, `split()`과 `join()` 메서드를 사용하여 문자를 배열로 분리하고, 필요없는 문자를 제거한 후 다시 문자열로 조합하는 방식을 사용할 수도 있습니다.

## 참조:
- [정규 표현식 MDN 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript 문자열 메서드](https://www.typescriptlang.org/docs/)
- [JavaScript에서 패턴 삭제 방법](https://www.geeksforgeeks.org/how-to-remove-a-character-from-string-in-javascript/)