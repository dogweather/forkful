---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

문자열 추출은 문자열의 일부를 선택하는 것을 의미합니다. 많은 프로그래밍 작업이 특정 문자열에서 일부 데이터를 가져와 데이터를 파싱하고 검사하는 것에 의존하기 때문에, 프로그래머들은 이 작업을 매우 자주 실행합니다.

## 어떻게:

TypeScript에서는 주로 두가지 방법으로 문자열을 추출할 수 있습니다. `substring()` 과 `slice()` 함수입니다.

```TypeScript
let str = "Hello World!";

let substr = str.substring(1,4); //결과: "ell"
let slic = str.slice(1,4);  // 결과: "ell"
```
이 두 함수가 어떻게 동작하는지 변화된 결과값으로 확인해 볼 수 있습니다.

## 깊게 알아보기:

`substring()` 과 `slice()` 두 함수는 자바스크립트에서 최초로 도입되었고, TypeScript에도 포함되어 있습니다. 둘 다 비슷한 기능을 제공하지만, 음수 인자 처리 방식에서 차이가 있습니다. `substring()`은 음수 인자를 0으로 처리하며, `slice()`는 문자열의 끝에서부터 세어집니다.

더 나아가, `substr()` 함수는 시작 인덱스와 알파벳 길이를 사용하는 또 다른 방법으로 문자열을 추출하는 데 사용될 수 있습니다. 하지만 ECMAScript 2015 이후로 이 함수는 비추천 상태(deprecated)이므로 가능한 `substring()`이나 `slice()`를 사용하는 것이 좋습니다.

## 참조자료:

- [Mozilla Developer Network의 `substring()` 가이드](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Mozilla Developer Network의 `slice()` 가이드](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [W3Schools의 `substr()` 과 관련된 글](https://www.w3schools.com/jsref/jsref_substr.asp)