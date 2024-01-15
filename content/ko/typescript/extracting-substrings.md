---
title:                "부분 문자열 추출"
html_title:           "TypeScript: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

왜 사람들이 하위 문자열을 추출하는 것에 참여하게 될까요? 하위 문자열 추출은 문자열을 더 작은 부분으로 분할하여 정보를 일반적인 형태로 변환하고 원하는 결과를 얻기 위해 사용됩니다. 이는 많은 개발자들에게 매우 유용한 기술이며 그들의 코드를 더 간결하고 효율적으로 만들어 줍니다.

## 어떻게

```TypeScript
// 기본 문자열
let str = "안녕하세요! 반가워요.";

// substr() 함수를 사용하여 "안녕하세요!"라는 부분을 추출합니다.
let result = str.substr(0, 6);

// 결과 출력
console.log(result) // "안녕하세요!"
```

위의 예제는 TypeScript의 substr() 함수를 사용하여 문자열의 일부분을 추출하는 방법을 보여줍니다. substr() 함수는 두 개의 매개변수를 사용하는데, 첫 번째 매개변수는 추출할 문자열의 시작 인덱스를 나타내고 두 번째 매개변수는 추출할 문자열의 길이를 나타냅니다.

## 깊이 파고들기

하위 문자열 추출은 문자열 관련 작업을 수행하는 동안 매우 유용한 기술입니다. substr() 함수를 비롯한 다양한 메소드를 사용하여 문자열을 다양한 방식으로 추출할 수 있습니다. 예를 들어, substring() 함수는 시작 인덱스와 종료 인덱스를 사용하여 문자열의 일부분을 추출하는 데 사용될 수 있습니다. 또한 정규식을 사용하여 더 복잡한 문자열 추출 작업을 수행할 수도 있습니다.

See Also:

- [substr() 메소드 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [substring() 메소드 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substring)