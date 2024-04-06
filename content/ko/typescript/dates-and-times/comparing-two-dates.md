---
date: 2024-01-20 17:34:12.777141-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uB0A0\uC9DC\uB97C \uBE44\
  \uAD50\uD558\uB294 \uAC83\uC740 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8/\uD0C0\uC785\
  \uC2A4\uD06C\uB9BD\uD2B8\uC758 \uCD08\uCC3D\uAE30\uBD80\uD130 \uC8FC\uC694 \uAE30\
  \uB2A5\uC774\uC5C8\uC2B5\uB2C8\uB2E4. `.getTime()` \uBA54\uC11C\uB4DC\uB97C \uC0AC\
  \uC6A9\uD558\uBA74 \uB0A0\uC9DC \uAC1D\uCCB4\uB97C \uBC00\uB9AC\uCD08\uB85C \uBCC0\
  \uD658\uD558\uC5EC \uC815\uD655\uD55C \uBE44\uAD50\uAC00 \uAC00\uB2A5\uD569\uB2C8\
  \uB2E4. \uB2E4\uB978 \uBC29\uBC95\uB4E4\uB3C4 \uC874\uC7AC\uD569\uB2C8\uB2E4. \uC608\
  \uB97C \uB4E4\uC5B4, \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB85C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.672221-06:00'
model: gpt-4-1106-preview
summary: "`.getTime()` \uBA54\uC11C\uB4DC\uB97C \uC0AC\uC6A9\uD558\uBA74 \uB0A0\uC9DC\
  \ \uAC1D\uCCB4\uB97C \uBC00\uB9AC\uCD08\uB85C \uBCC0\uD658\uD558\uC5EC \uC815\uD655\
  \uD55C \uBE44\uAD50\uAC00 \uAC00\uB2A5\uD569\uB2C8\uB2E4."
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
