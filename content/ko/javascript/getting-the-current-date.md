---
title:                "Javascript: 현재 날짜 가져오기"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
날짜와 시간은 우리 일상 생활에서 매우 중요한 역할을 합니다. 우리는 날짜를 기반으로 회의 일정을 조율하고, 생일을 축하하며, 일정 계획을 세우는 등 다양한 상황에서 날짜를 필요로 합니다. 따라서 현재 날짜를 얻는 것은 매우 중요한 프로그래밍 작업입니다.

## 방법
자바스크립트에서 현재 날짜를 얻는 방법은 간단합니다. `Date()` 함수를 사용하면 됩니다. 아래의 예제 코드를 확인해보세요.

```Javascript
// 현재 날짜를 얻는 방법
let currentDate = new Date();

// 오늘의 연도를 얻는 방법
let year = currentDate.getFullYear();
console.log(year); // 출력 결과: 2021

// 오늘의 월을 얻는 방법 
// (0부터 시작하는 것에 유의하세요. 즉 0은 1월을 의미합니다.)
let month = currentDate.getMonth() + 1;
console.log(month); // 출력 결과: 5

// 오늘의 일을 얻는 방법
let date = currentDate.getDate();
console.log(date); // 출력 결과: 25

// 오늘의 요일을 얻는 방법
let day = currentDate.getDay();
console.log(day); // 출력 결과: 4 (목요일을 의미합니다.)
```

위의 코드에서 `new Date()`는 현재 시간을 나타냅니다. `getYear()`, `getMonth()`, `getDate()` 함수를 사용하여 각각 연도, 월, 일을 얻을 수 있습니다. 또한 `getDay()` 함수를 사용하면 현재 날짜의 요일을 숫자로 반환합니다. 따라서 이러한 함수를 조합하여 원하는 형식으로 현재 날짜를 출력할 수 있습니다. 

## 딥 다이브
`Date()` 함수를 사용하여 현재 날짜를 얻는 방법에는 여러 가지가 있습니다. 위의 예제에서는 가장 기본적인 방법을 소개했지만, `Date()` 함수에는 다양한 기능들이 있습니다. 예를 들어 `Date()` 함수를 통해 시간, 분, 초 등 다양한 시간 정보도 얻을 수 있습니다. 또한 파라미터를 이용하여 특정 날짜부터 경과한 시간을 계산할 수도 있습니다. 이러한 함수들은 각각 다른 자바스크립트 함수와 조합하여 다양한 기능을 구현할 수 있습니다.

## 참고하기
- [Date() 함수 문서 (MDN)](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [날짜와 시간 다루기 (W3Schools)](https://www.w3schools.com/js/js_date_methods.asp)
- [자바스크립트 시간과 날짜 다루기 (Guru99)](https://www.guru99.com/date-time-and-calendar.html)