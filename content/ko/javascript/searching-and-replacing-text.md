---
title:    "Javascript: 텍스트 검색 및 대체"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜
많은 프로그래밍 작업에서, 문자열을 변경하고 다른 내용으로 대체하는 것은 매우 중요합니다. 이를 효율적으로 수행하는 방법을 배우는 것은 모든 개발자에게 필수적인 기술입니다.

## 하는 법
```Javascript
// 예시 문자열
let str = "안녕하세요, 제 이름은 영희입니다.";

// "영희"를 "민수"로 대체하는 예제
let replacedStr = str.replaceAll("영희", "민수");

// 콘솔에 출력
console.log(replacedStr);
```

이 코드를 실행하면 "안녕하세요, 제 이름은 민수입니다."라는 결과가 나타납니다.

## 깊게 파헤치기
문자열의 모든 인스턴스를 변경하는 데에는 `replaceAll()` 함수를 사용할 수 있지만, 특정 인덱스에 있는 문자만 변경하고 싶다면 `replace()` 함수를 사용해야 합니다. 또한 정규식을 사용해 더 복잡한 패턴의 문자열을 대체할 수도 있습니다.

## 더 알아보기
[문자열 대체에 대한 더 많은 정보](https://www.w3schools.com/jsref/jsref_replace.asp)를 참고해보세요.

## 참고하기
- [자바스크립트 문자열 다루기](https://www.w3schools.com/js/js_strings.asp)
- [정규식을 사용한 문자열 다루기](https://www.regular-expressions.info/javascript.html)