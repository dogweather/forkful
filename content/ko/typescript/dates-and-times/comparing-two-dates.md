---
date: 2024-01-20 17:34:12.777141-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.871324-06:00'
model: gpt-4-1106-preview
summary: .
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to (어떻게 하나요?)
```TypeScript
const date1 = new Date('2023-03-25T09:00:00');
const date2 = new Date('2023-03-25T17:00:00');

// 날짜 비교: 이전, 이후, 동일한지 체크하기
console.log(date1 < date2);  // true - date1이 이전
console.log(date1 > date2);  // false - date1이 이후가 아님
console.log(date1.getTime() === date2.getTime());  // false - 동일한 시각이 아님

// 시간 차이 계산하기 (밀리초 단위)
const diff = date2.getTime() - date1.getTime();
console.log(diff);  // 28800000

// '날(day)' 단위로 변환해서 시간 차이 표시하기
const diffInDays = diff / (1000 * 60 * 60 * 24);
console.log(diffInDays);  // 0.3333333333333333
```

## Deep Dive (심층 분석)
날짜를 비교하는 것은 자바스크립트/타입스크립트의 초창기부터 주요 기능이었습니다. `.getTime()` 메서드를 사용하면 날짜 객체를 밀리초로 변환하여 정확한 비교가 가능합니다. 

다른 방법들도 존재합니다. 예를 들어, 라이브러리로 Moment.js가 있지만, 현재는 Day.js나 date-fns 같은 더 가벼운 대안들이 선호됩니다. 이들 라이브러리는 구문이 간단하고, 국제화 지원이 잘 되어 있습니다.

내부적으로, 날짜 비교는 유닉스 타임스탬프로 날짜를 나타내는 정수 비교로 간소화됩니다. 이것은 날짜가 1970년 1월 1일부터 몇 밀리초 지났는지를 계산한 값입니다.

## See Also (추가자료)
- MDN Date Reference: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Day.js: [Day.js Documentation](https://day.js.org)
- date-fns: [date-fns Documentation](https://date-fns.org)
