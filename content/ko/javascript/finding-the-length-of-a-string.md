---
title:                "문자열의 길이를 찾기"
html_title:           "Javascript: 문자열의 길이를 찾기"
simple_title:         "문자열의 길이를 찾기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 길이를 찾는 것은 프로그래머들이 자주 해야 하는 작업 중 하나입니다. 이 작업은 특정 문자열 내에 포함된 문자의 수를 알고 싶을 때 사용됩니다. 이를 통해 우리는 인풋 데이터의 양을 파악하고, 유효성 검사를 할 수 있습니다.

## 하는 법:
```Javascript
// 문자열의 길이를 찾는 함수
var stringLength = function(string) {

  // 문자열의 길이를 저장할 변수 선언
  var length = 0;

  // 문자열을 한 글자씩 순회하며 길이 증가
  for (var i = 0; i < string.length; i++) {
    length++;
  }

  // 최종 길이 값 반환
  return length;
};

// 함수 실행
console.log(stringLength("안녕하세요!"));
```

출력 값: 6

## 딥 다이브:
1. 과거: 문자열의 길이를 찾는 방법은 컴퓨터가 등장한 초기부터 사용되어 왔습니다. 단순하면서도 유용한 작업이기 때문에 여전히 자주 사용되고 있습니다.
2. 대안: Javascript에서는 내장함수인 .length를 사용해 문자열의 길이를 찾을 수 있습니다. 또한 다른 프로그래밍 언어, 예를 들어 Python에서는 len()이라는 내장함수를 제공합니다.
3. 구현 방법: 우리 코드에서 사용한 것처럼 문자열을 순회하며 길이를 증가시켜주는 방식으로 문자열의 길이를 찾을 수 있습니다. 또한 Javascript에서는 문자열 뿐만 아니라 배열, 객체 등의 길이도 쉽게 찾을 수 있습니다.

## 더 알아보기:
- [JavaScript 문자열의 길이 구하기](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [자바스크립트 문자열의 길이 구하기](https://velog.io/@kim-macbook/JavaScript)
- [JavaScript 문자열, 배열, 객체 길이 구하기](https://goddaehee.tistory.com/52)