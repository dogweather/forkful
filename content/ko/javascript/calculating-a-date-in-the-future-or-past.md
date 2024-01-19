---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Javascript: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

날짜 계산은 특정 날짜에서 시간을 더하거나 빼서 미래나 과거의 날짜를 구하는 것입니다. 예약 기능 구현, 휴가 추적, 전역 날짜 계산 등의 용도로 개발자들이 주로 이용합니다.

## 어떻게 하나:

자바스크립트에서 날짜를 계산하는 방법은 여러 가지가 있습니다. 가장 간단한 방법은 `Date` 객체를 사용하는 것입니다.

```Javascript
// 오늘 날짜 구하기
var today = new Date();

// 7일 후의 날짜 구하기
var future = new Date();
future.setDate(today.getDate() + 7);

console.log("Today is: " + today);
console.log("7 days from now is: " + future);
```

이 코드에서는 `setDate()` 함수를 사용해 오늘 날짜에 7일을 더하였습니다.

## 깊이 들여다보기:

날짜 계산이 발전하게 된 배경에는 컴퓨터의 시간과 날짜 처리 방식이 큰 영향을 미쳤습니다. Unix 시간 (1970년 1월 1일부터 현재까지의 초)이 알려지며, 이 원리를 기반으로 JavaScript Date 객체가 문제없이 작동합니다.

앞서 소개한 방법 말고도 `setTime()`나 `getTime()` 등의 메소드를 사용하여 계산할 수 있습니다.

```Javascript
// 오늘 날짜 구하기
var today = new Date();

// 7일 후 날짜 구하기 (밀리초 단위로 계산)
// 1일은 24 * 60 * 60 * 1000 = 86400000 밀리초
var future = new Date(today.getTime() + 7 * 86400000);

console.log("Today is: " + today);
console.log("7 days from now is: " + future);
```
하지만, 변경 가능성을 고려하면 충분히 더 가독성이 좋고 직관적인 `setDate()`를 사용하는 것이 더 효과적일 수 있습니다.

## 참고 자료:

- JavaScript Date 객체: https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date
- Unix 시간: https://ko.wikipedia.org/wiki/유닉스_시간
- JavaScript setDate() 메소드: https://www.w3schools.com/jsref/jsref_setdate.asp