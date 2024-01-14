---
title:    "Javascript: 날짜를 문자열로 변환하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환할 이유는 무엇일까요? 자바스크립트에서 날짜를 다루는 방법을 배울 때, 날짜를 문자열로 변환하는 것은 매우 중요합니다. 이 기능을 알아두면 프로그래밍에서 유용하게 사용할 수 있습니다.

## 어떻게
날짜를 문자열로 변환하는 방법을 알아보겠습니다. 먼저 `toString()` 메서드를 사용하여 날짜 객체를 문자열로 변환할 수 있습니다. 아래는 예제 코드입니다.

```Javascript
const today = new Date();
const dateString = today.toString();
console.log(dateString); // Wed Sep 09 2020 18:49:37 GMT+0900 (Korean Standard Time)
```

또는 `getFullYear()`, `getMonth()`, `getDate()`, `getHours()`, `getMinutes()`, `getSeconds()` 메서드를 사용하여 개별 날짜 요소를 추출한 후, 문자열로 변환할 수 있습니다. 아래는 예제 코드입니다.

```Javascript
const today = new Date();
const year = today.getFullYear();
const month = String(today.getMonth() + 1).padStart(2, '0');
const date = String(today.getDate()).padStart(2, '0');
const hours = String(today.getHours()).padStart(2, '0');
const minutes = String(today.getMinutes()).padStart(2, '0');
const seconds = String(today.getSeconds()).padStart(2, '0');
const dateString = `${year}-${month}-${date} ${hours}:${minutes}:${seconds}`;
console.log(dateString); // 2020-09-09 18:49:37
```

## 깊게 파보기
날짜를 문자열로 변환하는 방법을 자세히 살펴보겠습니다. 자바스크립트에서는 날짜를 표현하는 다양한 형식이 있습니다. `toString()` 메서드를 사용하면 표준 시간대의 날짜 및 시간 정보와 함께 문자열로 변환됩니다. `getFullYear()` 메서드는 4자리 연도를 가져오고, `getMonth()` 메서드는 0부터 시작하는 월을 가져옵니다. 이러한 메서드들을 조합하여 원하는 형식의 문자열로 만들 수 있습니다.

아래는 다양한 형식의 날짜를 문자열로 변환하는 예제 코드입니다.

```Javascript
const today = new Date();
const fullDate = `${today.getFullYear()}년 ${today.getMonth() + 1}월 ${today.getDate()}일`;
const dayOfWeek = ['일', '월', '화', '수', '목', '금', '토'][today.getDay()];
const fullTime = `${today.getHours()}시 ${today.getMinutes()}분 ${today.getSeconds()}초`;
console.log(`오늘은 ${fullDate} (${dayOfWeek})이며, 현재 시간은 ${fullTime} 입니다.`); // 오늘은 2020년 9월 9일 (수)이며, 현재 시간은 18시 49분 37초 입니다.
```

## 더 알아보기
이외에도 다양한 날짜 정보를 문자열로 변환하는 방법이 있습니다. `toDateString()`, `toTimeString()`, `toLocaleDateString()` 등의 메서드를 사용하여 원하는 형식으로 날짜를 문자열로 변환할 수 있습니다.

## 더 많은 자료
- [MDN Web Docs - Date.prototype.toString()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN Web Docs - Date.prototype.getFullYear()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date/getFullYear)
- [MDN Web Docs - Array.prototype.padStart()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Array/padStart)
- [MDN Web Docs - Date Format Strings](https://