---
title:                "부분 문자열 추출하기"
html_title:           "Javascript: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜?

문자열의 일부분을 추출하는 것은 자바스크립트 프로그래밍에서 매우 유용한 기술입니다. 우리는 자주 문자열을 다루는데 그만큼 이 기술을 이해하고 활용하는 것이 중요합니다.

## 어떻게?

자바스크립트에서 문자열의 일부분을 추출하는 방법에 대해 알아보겠습니다. 아래의 예제 코드를 살펴보고 결과를 확인해보세요.

```javascript
// 문자열 선언
let str = "안녕하세요! 이번에는 자바스크립트에 대해 알아보려고 합니다.";

// 일부분 추출
let sub = str.substring(7, 12);

// 결과 출력
console.log(sub); // '이번에는'
```

위 코드에서는 `substring()` 함수를 사용하여 원하는 부분의 문자열을 추출하고 있습니다. 첫 번째 매개변수는 시작 인덱스, 두 번째 매개변수는 종료 인덱스를 나타냅니다. 또한 이 함수는 원본 문자열을 수정하지 않고 추출한 부분을 새로운 문자열로 반환합니다.

## 깊이 파헤치기

우리는 `substring()` 함수를 사용하여 문자열의 일부분을 추출할 수 있지만, 더 깊게 파헤쳐보겠습니다. 이 함수는 첫 번째 매개변수로 음수 값을 사용할 수 있습니다. 음수 값을 사용하면 문자열의 끝에서부터 인덱싱을 시작합니다.

```javascript
// 문자열 선언
let str = "안녕하세요! 이번에는 자바스크립트에 대해 알아보려고 합니다.";

// 뒤에서 8글자 추출
let sub = str.substring(-8);

// 결과 출력
console.log(sub); // '배알때려'
```

또한, `substring()` 함수 대신 `slice()` 함수를 사용하여 문자열의 일부분을 추출할 수도 있습니다. `slice()` 함수는 시작 인덱스만 매개변수로 받는데, 이는 `substring()` 함수의 두 번째 매개변수를 생략한 것과 같습니다.

```javascript
// 문자열 선언
let str = "자바스크립트 짱짱맨!";

// 일부분 추출
let sub = str.slice(8);

// 결과 출력
console.log(sub); // '짱맨!'
```

## 관련 링크

- [MDN substring()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN slice()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/slice)