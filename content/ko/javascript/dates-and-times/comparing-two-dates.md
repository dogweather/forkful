---
date: 2024-01-20 17:33:21.962496-07:00
description: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uB450 \uB0A0\uC9DC\
  \uB97C \uBE44\uAD50\uD558\uB294 \uAC83\uC740 \uD2B9\uC815\uD55C \uC2DC\uAC04\uC774\
  \ \uC804\uC778\uC9C0, \uD6C4\uC778\uC9C0, \uAC19\uC740\uC9C0\uB97C \uD655\uC778\uD558\
  \uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uAE30\uAC04\uC744 \uCE21\uC815\uD558\uAC70\uB098, \uB0A0\uC9DC \uAE30\uBC18 \uC758\
  \uC0AC\uACB0\uC815\uC744 \uB0B4\uB9AC\uAE30 \uC704\uD574 \uC774\uB97C \uC0AC\uC6A9\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.808841-06:00'
model: gpt-4-1106-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C \uB450 \uB0A0\uC9DC\uB97C\
  \ \uBE44\uAD50\uD558\uB294 \uAC83\uC740 \uD2B9\uC815\uD55C \uC2DC\uAC04\uC774 \uC804\
  \uC778\uC9C0, \uD6C4\uC778\uC9C0, \uAC19\uC740\uC9C0\uB97C \uD655\uC778\uD558\uB294\
  \ \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to: (어떻게 하나요?)
```Javascript
// 두 날짜 객체 생성
const date1 = new Date('2023-04-01T00:00:00');
const date2 = new Date('2023-04-02T00:00:00');

// 날짜 비교
console.log(date1 < date2); // true, date1이 date2보다 이릅니다
console.log(date1 > date2); // false, date1이 date2보다 늦습니다
console.log(date1.getTime() === date2.getTime()); // false, 두 날짜는 같지 않습니다
```

## Deep Dive (심층 분석)
- **역사적 맥락**: 자바스크립트의 Date 객체는 1995년 언어가 처음 만들어질 때부터 존재했습니다. 
- **대안**: 라이브러리 없이 날짜를 비교할 수 있지만, moment.js와 같은 라이브러리를 사용하면 편리합니다.
- **구현 세부 사항**: 자바스크립트에서는 `getTime()` 메서드를 사용해 유닉스 타임스탬프로 변환하여 날짜를 비교합니다. 이 값은 1970년 1월 1일부터의 밀리초 수를 나타냅니다.

## See Also (더 보기)
- [Date 객체 - MDN Web Docs](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [Date-fns library](https://date-fns.org/)
