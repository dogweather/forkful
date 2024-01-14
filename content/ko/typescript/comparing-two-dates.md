---
title:                "TypeScript: 두 날짜 비교하기"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜 두 날짜를 비교하는가?

컴퓨터 프로그래밍에서 날짜는 매우 중요합니다. 예를 들어, 이전 날짜에 대한 정보를 저장하고 특정 날짜와 비교하여 지난 시간을 계산하거나, 미래의 날짜를 예측하는 등의 작업을 할 수 있습니다. 따라서 두 날짜를 비교하는 것은 매우 유용한 기능입니다.

## 어떻게 비교할까?

TypeScript에서 두 날짜를 비교하는 방법을 알아보겠습니다. 먼저, `Date` 객체를 사용하여 날짜를 생성합니다. 그리고 `getTime()` 메소드를 이용해서 두 날짜를 milliseconds로 변환합니다. 마지막으로 `if` 문을 사용하여 두 날짜를 비교할 수 있습니다.

```TypeScript
let date1: Date = new Date("March 17, 2021");
let date2: Date = new Date("March 18, 2021");

if (date1.getTime() > date2.getTime()) {
  console.log("date1 is later than date2");
} else {
  console.log("date2 is later than date1")
}

// 출력: date2 is later than date1
```

더 복잡한 비교를 위해서는 `Date` 객체의 `getFullYear()`, `getMonth()`, `getDate()` 메소드를 사용하여 날짜의 연도, 월, 일을 비교할 수 있습니다. 예를 들어, 두 날짜가 같은지 비교해보겠습니다.

```TypeScript
let date3: Date = new Date("March 17, 2021");
let date4: Date = new Date("March 17, 2021");

if (date3.getFullYear() === date4.getFullYear() && date3.getMonth() === date4.getMonth() && date3.getDate() === date4.getDate()) {
  console.log("date3 and date4 are the same");
}

// 출력: date3 and date4 are the same
```

## 더 깊이 들어가보기

날짜를 비교할 때 유의해야 할 점은 날짜에 대한 정보만을 고려하는 것이 아니라, 시간대(Time Zone)와 로케일(Locale)에 따라 결과가 달라질 수 있다는 것입니다. 따라서 정확한 비교를 위해서는 `getTimezoneOffset()` 메소드를 사용하여 시간대를 고려해야 합니다.

또한, 날짜 형식을 파싱할 때는 `utc` 메소드를 사용하여 시간대를 `UTC`(협정 세계시)로 설정하는 것이 좋습니다. 이를 통해 날짜 형식을 일관성 있게 유지할 수 있습니다.

## 관련 링크

- [MDN: Comparing Dates in JavaScript](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript 공식 문서: Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-7.html#more-accurately-reflect-ecmascript)
- [TypeScript 날짜 라이브러리 Moment.js](https://momentjs.com/)