---
date: 2024-01-20 17:38:00.541678-07:00
description: "How to (\uBC29\uBC95) \uD0C0\uC785\uC2A4\uD06C\uB9BD\uD2B8\uB294 \uC790\
  \uBC14\uC2A4\uD06C\uB9BD\uD2B8\uB97C \uAE30\uBC18\uC73C\uB85C \uD558\uBBC0\uB85C\
  \ \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294 \uC5EC\uB7EC\
  \ \uBA54\uC11C\uB4DC\uB294 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC758 \uC5ED\uC0AC\
  \uC640 \uD568\uAED8 \uC2DC\uC791\uB429\uB2C8\uB2E4. \uC608\uB97C \uB4E4\uC5B4 `toString()`\uC740\
  \ \uAC00\uC7A5 \uAE30\uBCF8\uC801\uC778 \uBA54\uC11C\uB4DC\uB85C, \uC2DC\uC2A4\uD15C\
  \uC758 \uB85C\uCE98\uC5D0 \uB530\uB77C \uB2E4\uB978 \uD615\uC2DD\uC744 \uC81C\uACF5\
  \uD569\uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.671053-06:00'
model: gpt-4-1106-preview
summary: "\uC608\uB97C \uB4E4\uC5B4 `toString()`\uC740 \uAC00\uC7A5 \uAE30\uBCF8\uC801\
  \uC778 \uBA54\uC11C\uB4DC\uB85C, \uC2DC\uC2A4\uD15C\uC758 \uB85C\uCE98\uC5D0 \uB530\
  \uB77C \uB2E4\uB978 \uD615\uC2DD\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## How to (방법)
```typescript
// 날짜를 문자열로 기본 변환
let current = new Date();
console.log(current.toString()); // "Tue Mar 01 2022 16:20:00 GMT+0900 (Korean Standard Time)"

// toLocaleString을 사용해 현지 언어 포맷으로 변환
console.log(current.toLocaleString('ko-KR')); // "2022. 3. 1. 오후 4:20:00"

// toISOString을 사용해 ISO 8601 포맷으로 변환
console.log(current.toISOString()); // "2022-03-01T07:20:00.000Z"
```

## Deep Dive (심층 분석)
타입스크립트는 자바스크립트를 기반으로 하므로 날짜를 문자열로 변환하는 여러 메서드는 자바스크립트의 역사와 함께 시작됩니다. 예를 들어 `toString()`은 가장 기본적인 메서드로, 시스템의 로캘에 따라 다른 형식을 제공합니다.

`toLocaleString()` 메서드는 더 많은 제어를 제공합니다. 언어 및 지역 설정으로 나타날 형식을 결정할 수 있습니다. 예를 들어, `'ko-KR'`은 한국식 날짜 포맷입니다.

`toISOString()` 메서드는 상호 운용성을 위해 설계되었습니다. 모든 시스템에서 똑같은 형식(ISO 8601)을 사용합니다.

외부 라이브러리(예: Moment.js, Date-fns)를 사용하면 날짜를 문자열로 변환할 때 추가적인 포맷 옵션과 편의성을 얻을 수 있지만 최근에는 `Intl.DateTimeFormat` 같은 내장 API가 강화되면서 외부 라이브러리의 필요성이 줄어들고 있습니다.

## See Also (참고 자료)
- MDN Web Docs: Date.toString() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString
- MDN Web Docs: Date.toLocaleString() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString
- MDN Web Docs: Date.toISOString() - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString
- TypeScript Official Documentation - https://www.typescriptlang.org/docs
- Intl.DateTimeFormat - https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat
