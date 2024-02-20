---
date: 2024-01-20 17:32:30.736786-07:00
description: "\uB0A0\uC9DC \uACC4\uC0B0\uC740 \uBBF8\uB798\uB098 \uACFC\uAC70\uC758\
  \ \uD2B9\uC815 \uB0A0\uC9DC\uB97C \uCC3E\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC608\uC57D\
  \ \uC2DC\uC2A4\uD15C, \uAE30\uD55C \uC124\uC815, \uC0AC\uAC74 \uCD94\uC801 \uB4F1\
  \uC5D0 \uC4F0\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:13.765889
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC \uACC4\uC0B0\uC740 \uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uD2B9\
  \uC815 \uB0A0\uC9DC\uB97C \uCC3E\uB294 \uAC83\uC785\uB2C8\uB2E4. \uC608\uC57D \uC2DC\
  \uC2A4\uD15C, \uAE30\uD55C \uC124\uC815, \uC0AC\uAC74 \uCD94\uC801 \uB4F1\uC5D0\
  \ \uC4F0\uC785\uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 계산은 미래나 과거의 특정 날짜를 찾는 것입니다. 예약 시스템, 기한 설정, 사건 추적 등에 쓰입니다.

## How to: (방법)
```TypeScript
// 오늘부터 10일 후의 날짜
const today = new Date();
const tenDaysLater = new Date(today.getTime() + 10 * 24 * 60 * 60 * 1000);
console.log(tenDaysLater.toDateString()); // "Mar 01 2023"

// 오늘부터 10일 전의 날짜
const tenDaysBefore = new Date(today.getTime() - 10 * 24 * 60 * 60 * 1000);
console.log(tenDaysBefore.toDateString()); // "Feb 09 2023"
```

## Deep Dive (심층 분석)
시간은 언제나 중요했습니다. 과거의 날짜 계산은 주로 달력과 수학으로 했어요. 컴퓨터가 보급되면서 자바스크립트와 같은 프로그래밍 언어가 날짜 계산을 간단히 만들어 줬습니다. 한국에서는 양력달력 외에도 음력달력 계산에 종종 쓰입니다. 

`Date` 객체 외에도 `moment.js`, `date-fns` 같은 라이브러리가 있어 더 복잡한 날짜 처리를 도와줍니다. 그러나 TypeScript에서 원시 `Date` 객체만 이용해도 기본적인 날짜 계산은 충분합니다.

날짜 계산 시, 윤년이나 시간대, 서머타임과 같은 복잡한 상황을 고려해야 할 때가 종종 있습니다.

## See Also (참고자료)
- MDN Web Docs Date reference: [MDN Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- date-fns library: [date-fns](https://date-fns.org/)
- moment.js library: [Moment.js](https://momentjs.com/)

이러한 자료들은 날짜와 관련된 깊은 지식과 추가적인 기능을 제공합니다.
