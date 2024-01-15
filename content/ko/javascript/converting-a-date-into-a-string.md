---
title:                "날짜를 문자열로 변환하는 방법"
html_title:           "Javascript: 날짜를 문자열로 변환하는 방법"
simple_title:         "날짜를 문자열로 변환하는 방법"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 일은 자바스크립트 프로그래밍에서 매우 중요합니다. 이를 통해 날짜를 보다 쉽게 다룰 수 있고, 다양한 방식으로 활용할 수 있습니다.

## 어떻게
아래는 날짜를 문자열로 변환하는 방법의 예시 코드와 출력 결과입니다.

```Javascript
// 현재 날짜를 가져오는 Date 객체 생성
let today = new Date();

// toUTCString 메소드를 사용하여 UTC 표준 시간 형식으로 변환
console.log(today.toUTCString()); // 출력: Wed, 27 Oct 2021 07:00:00 GMT

// toLocaleDateString 메소드를 사용하여 지역 시간 형식으로 변환
console.log(today.toLocaleDateString()); // 출력: 10/27/2021
```

위 코드 예시에서 볼 수 있듯이, 자바스크립트에서는 Date 객체의 내장 메소드를 사용하여 매우 쉽게 날짜를 문자열로 변환할 수 있습니다.

## 딥 다이브
자바스크립트에서 날짜를 문자열로 변환하는 방법은 다양합니다. 위의 예시에서 사용한 `toUTCString`과 `toLocaleDateString` 외에도 `toDateString`, `toISOString` 등 다양한 메소드가 있습니다. 또한, 변환된 문자열의 포맷을 조절할 수 있는 방법도 있으니, 자세한 내용은 아래 링크들을 참고해 보시기 바랍니다.

## 관련 링크
- [자바스크립트 공식 문서: Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [날짜를 문자열로 변환하는 다양한 방법](https://www.w3schools.com/js/js_date_formats.asp)
- [자바스크립트에서의 지역 시간과 UTC 시간의 차이](https://stackoverflow.com/questions/7370542/what-is-the-significance-of-appending-z-to-date-strings-in-javascript)
- [날짜 포맷을 조절하는 방법](https://momentjs.com/docs/#/displaying/)