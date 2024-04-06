---
date: 2024-01-20 17:32:30.736786-07:00
description: "How to: (\uBC29\uBC95) \uC2DC\uAC04\uC740 \uC5B8\uC81C\uB098 \uC911\uC694\
  \uD588\uC2B5\uB2C8\uB2E4. \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uC740 \uC8FC\
  \uB85C \uB2EC\uB825\uACFC \uC218\uD559\uC73C\uB85C \uD588\uC5B4\uC694. \uCEF4\uD4E8\
  \uD130\uAC00 \uBCF4\uAE09\uB418\uBA74\uC11C \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \uC640 \uAC19\uC740 \uD504\uB85C\uADF8\uB798\uBC0D \uC5B8\uC5B4\uAC00 \uB0A0\uC9DC\
  \ \uACC4\uC0B0\uC744 \uAC04\uB2E8\uD788 \uB9CC\uB4E4\uC5B4 \uC92C\uC2B5\uB2C8\uB2E4\
  . \uD55C\uAD6D\uC5D0\uC11C\uB294 \uC591\uB825\uB2EC\uB825 \uC678\uC5D0\uB3C4 \uC74C\
  \uB825\uB2EC\uB825 \uACC4\uC0B0\uC5D0 \uC885\uC885 \uC4F0\uC785\uB2C8\uB2E4. `Date`\
  \ \uAC1D\uCCB4\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.673296-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) \uC2DC\uAC04\uC740 \uC5B8\uC81C\uB098 \uC911\uC694\uD588\uC2B5\
  \uB2C8\uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

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
