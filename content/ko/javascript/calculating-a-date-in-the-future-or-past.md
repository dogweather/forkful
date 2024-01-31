---
title:                "미래나 과거의 날짜 계산하기"
date:                  2024-01-20T17:31:30.549584-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

category:             "Javascript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
날짜 계산은 미래나 과거의 특정 날짜를 찾는 것입니다. 예약 시스템, 할인 쿠폰 만료일 확인, 또는 경과 시간 추적처럼, 시간에 따라 달라지는 기능을 구현할 때 사용합니다.

## How to:
자바스크립트에서 날짜를 계산하는 방법은 `Date` 객체와 메소드를 이용합니다. 아래 예제를 확인해 보세요.

```javascript
// 오늘 날짜를 기준으로 1주 뒤의 날짜 계산하기
let today = new Date();
let nextWeek = new Date(today);
nextWeek.setDate(today.getDate() + 7);

console.log(nextWeek); // 1주 후 날짜 출력

// 3일 전 날짜 구하기
let threeDaysAgo = new Date(today);
threeDaysAgo.setDate(today.getDate() - 3);

console.log(threeDaysAgo); // 3일 전 날짜 출력
```

## Deep Dive
과거에는 자바스크립트의 날짜 관련 기능이 제한적이었기 때문에 시간을 계산할 때 문제가 많았습니다. 그러나 `Date` 객체와 시간대를 처리하는 라이브러리들의 발전으로 지금은 비교적 쉽습니다. `Moment.js` 같은 라이브러리는 전에 많이 쓰였지만, 지금은 더 적은 의존성과 모던한 API를 제공하는 `date-fns`나 `Luxon` 같은 대안들이 인기가 많습니다. `Date` 객체로는 날짜와 시간을 더하거나 빼기 위해 `getDate()`, `setDate()`와 같은 메소드를 사용하며, 월이나 년도 같은 다른 단위도 유사한 메소드로 조작할 수 있습니다.

## See Also
- MDN Web Docs: [Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- `date-fns` 라이브러리: [date-fns.org](https://date-fns.org/)
- `Luxon` 라이브러리: [moment.github.io/luxon](https://moment.github.io/luxon/)
