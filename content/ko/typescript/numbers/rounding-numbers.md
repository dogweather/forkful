---
date: 2024-01-26 03:47:37.251219-07:00
description: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uD2B9\uC815 \uC815\uBC00\uB3C4\
  \uAE4C\uC9C0 \uC22B\uC790\uB97C \uC904\uC774\uB294 \uAC83\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C \uC218\uCE58 \uCD9C\uB825\uC744\
  \ \uC81C\uC5B4\uD558\uAC70\uB098, \uAC00\uB3C5\uC131\uC744 \uC704\uD574, \uD639\uC740\
  \ \uBD80\uB3D9 \uC18C\uC218\uC810 \uACB0\uACFC\uB97C \uB0B8 \uC5F0\uC0B0 \uD6C4\
  \ \uD2B9\uC815 \uC815\uBC00\uB3C4\uAC00 \uC694\uAD6C\uB420 \uB54C \uC0AC\uC6A9\uD569\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.845839-06:00'
model: gpt-4-0125-preview
summary: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uC740 \uD2B9\uC815 \uC815\uBC00\uB3C4\uAE4C\
  \uC9C0 \uC22B\uC790\uB97C \uC904\uC774\uB294 \uAC83\uC785\uB2C8\uB2E4."
title: "\uC22B\uC790 \uBC18\uC62C\uB9BC\uD558\uAE30"
weight: 13
---

## 방법:
TypeScript에서 반올림은 여러 방법을 사용하여 수행할 수 있습니다. 간단한 실행 방법은 다음과 같습니다:

```typescript
// Math.round는 가장 가까운 정수로 반올림합니다.
console.log(Math.round(1.5)); // 출력: 2

// Math.ceil은 가장 가까운 정수로 올림합니다.
console.log(Math.ceil(1.1)); // 출력: 2

// Math.floor는 가장 가까운 정수로 내림합니다.
console.log(Math.floor(1.8)); // 출력: 1

// toFixed는 고정된 소수점 자리수로 반올림합니다.
let num = 1.23456;
console.log(num.toFixed(2)); // 출력: "1.23"
// 주의: toFixed는 문자열을 반환합니다! 필요한 경우 parseFloat을 사용하여 다시 변환하세요.
console.log(parseFloat(num.toFixed(2))); // 출력: 1.23
```

## 심층 탐구
예전에는 초기 컴퓨터의 제한된 공간과 정밀도 문제 때문에 반올림이 필수였습니다. 오늘날, 부동 소수점 연산은 이진수로 숫자가 저장되는 방식 때문에 기묘한 결과를 낳을 수 있습니다. 반올림 대안으로는 내림, 올림, 및 소수점 이하를 반올림하지 않고 잘라내는 trunc가 있습니다.

내부 작동이 주목할 만합니다: `Math.round`는 "round half up" (일명 "상업적 반올림")을 따르며, `Math.floor`와 `Math.ceil`은 간단명료합니다. `toFixed`는 문자열을 반환하고 "round half to even" (일명 "은행가의 반올림")을 사용하여 반올림하기 때문에 예상치 못한 결과를 초래할 수 있으며, 특히 동일한 숫자를 여러 번 반올림할 때 편향을 줄이는 데 유용합니다.

## 참고하기
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE 부동 소수점 산술 표준 (IEEE 754)](https://ieeexplore.ieee.org/document/4610935)
