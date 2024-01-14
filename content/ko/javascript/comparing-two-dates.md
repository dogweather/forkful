---
title:    "Javascript: 두 날짜 비교하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜

날짜를 비교하는 것은 프로그래밍에서 매우 중요합니다. 우리는 종종 날짜를 비교하여 이전 날짜와 현재 날짜를 구분하거나, 두 날짜 사이의 차이를 계산하거나, 특정 날짜에 따라 작업을 수행해야 할 때가 있습니다. 따라서 두 날짜를 비교하는 것은 매우 유용하고 필요합니다.

# 방법

자바스크립트에서 두 날짜를 비교하는 가장 간단한 방법은 `Date` 객체의 `getTime()` 메서드를 사용하는 것입니다. 이 메서드는 1970년 1월 1일 자정부터 해당 날짜까지의 밀리초를 반환합니다. 즉, 두 날짜의 시간 간격을 비교할 수 있게 해줍니다.

```
let date1 = new Date("2020-01-01");
let date2 = new Date("2021-01-01");

//두 날짜를 비교하여 차이 계산
let difference = date2.getTime() - date1.getTime();

console.log(difference); // 31536000000 (밀리초 단위)
```

또 다른 방법은 `getTime()` 대신 `getTimezoneOffset()` 메서드를 사용하는 것입니다. 이를 이용하면 두 날짜 간의 시간대 차이를 계산할 수 있습니다.

```
let date1 = new Date("2020-01-01");
let date2 = new Date("2021-01-01");

//두 날짜의 시간대 차이 계산
let timeDifference = date2.getTimezoneOffset() - date1.getTimezoneOffset();

console.log(timeDifference); // 0 (분 단위)
```

# 깊이 파고들기

두 날짜를 비교할 때에는 주의해야 할 점이 있습니다. `getTime()` 메서드를 사용할 때에는 두 날짜가 같은 시간대에 있는지와 관계없이 밀리초로 비교하게 됩니다. 따라서 같은 날짜라도 시간대가 다르다면 차이가 발생할 수 있습니다. 이러한 경우에는 `getTimezoneOffset()` 메서드를 사용하여 시간대를 확인하고 비교해야 정확한 결과를 얻을 수 있습니다.

또한, `Date` 객체의 생성자에 전달하는 날짜 형식에 따라 결과가 달라질 수 있습니다. 이러한 경우에는 먼저 `getTime()` 메서드로 밀리초를 구한 다음에 원하는 형식으로 변환하여 비교하는 것이 좋습니다.

# 참고하기

- [MDN: Date 객체](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Date and Time in Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Javascript: Compare Two Dates](https://www.w3resource.com/javascript-exercises/javascript-basic-exercise-17.php#RUN)
- [이상한모임 블로그: Javascript Date 타입 다루기](https://blog.weirdx.io/post/17796)