---
title:                "부분 문자열 추출"
html_title:           "Javascript: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

문자열에서 부분 문자열을 추출한다는 것은, 여러분이 가진 문자열 중 일부분을 따로 떼내어 따로 관리하는 것을 의미합니다. 프로그래머들이 이를 하는 이유는, 문자열에서 원하는 정보를 쉽게 찾고 처리하기 위해서입니다.

## 하는 법:

```Javascript
// 예시 문자열
let str = "Hello World";

// 부분 문자열 추출
let subStr1 = str.substring(0, 5); // 출력: "Hello"
let subStr2 = str.substring(6); // 출력: "World"

// 문자열의 일부분을 대체하여 새로운 문자열 생성
let newStr = str.replace("Hello", "Hi"); // 출력: "Hi World"
```

## 깊이 파고들기:

- 역사적 맥락: 부분 문자열 추출은 초기 컴퓨터 시스템에서도 많이 사용되었으며, 지금도 많은 개발자들이 활용하고 있습니다.
- 대안: 부분 문자열을 다루는 다양한 메소드들이 존재하지만, ```substring()```과 ```replace()``` 메소드는 가장 범용적으로 사용되는 방식입니다.
- 구현 세부사항: 문자열에서 부분 문자열을 추출하거나 대체할 때, 인덱스 번호를 기준으로 동작합니다. 첫 번째 인자는 추출 시작 인덱스이고, 두 번째 인자는 추출을 멈추는 인덱스입니다. 대체하는 경우에는 첫 번째 인자에 바꿀 부분 문자열을, 두 번째 인자에는 바꿀 문자열을 적어줍니다.

## 참고 자료:

- [MDN 문서 - ```substring()``` 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN 문서 - ```replace()``` 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)