---
title:                "부분 문자열 추출"
html_title:           "Arduino: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가요?

문자열의 부분 문자열을 추출하는 것은, 그 이름에서 알 수 있듯이 문자열에서 원하는 부분을 얻어내는 작업을 가리킵니다. 이는 데이터를 빠르게 분석하고 조작하려는 프로그래머들에게 매우 필요한 기능입니다.

## 어떻게 하는가:

부분 문자열을 추출하는 데는 몇 가지 방법이 있습니다. 아래에 몇 가지 예제를 제공합니다.

```Javascript
let str = "안녕하세요, 자바스크립트!";
console.log(str.substring(0, 5));  // 결과: "안녕하세요"
```

```Javascript
let str = "안녕하세요, 자바스크립트!";
console.log(str.slice(0, 5));  // 결과: "안녕하세요"
```

```Javascript
let str = "안녕하세요, 자바스크립트!";
console.log(str.substr(0, 5));  // 결과: "안녕하세요"
```

## 깊게 알아보기:

**역사적 맥락**: JavaScript가 처음 등장했을 때에는 문자열 추출 기능이 제한적이었습니다. 그러나 ECMAScript 2015(ES6)이 등장하면서 `substring`, `slice`, `substr`과 같은 여러 메서드가 도입되었습니다.

**대안**: `substring`, `slice`, `substr` 외에도 `split` 기법을 사용하여 부분 문자열을 추출할 수도 있습니다.

```Javascript
let str = "안녕하세요, 자바스크립트!";
console.log(str.split(",")[0]);  // 결과: "안녕하세요"
```

**구현 세부 사항**: `substring`, `slice`, `substr`은 각각 약간 다른 방식으로 작동합니다. `substring`과 `slice`는 시작 인덱스와 종료 인덱스를 기준으로 작동하며, `substr`는 시작 인덱스와 추출할 길이를 기준으로 작동합니다.

## 관련 자료:

1. [MDN: 문자열 메서드와 속성](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)
2. [w3schools: JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)