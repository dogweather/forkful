---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:02.852651-07:00
description: "\uBC29\uBC95: TypeScript\uB294 JavaScript\uC758 \uC288\uD37C\uC14B\uC73C\
  \uB85C\uC11C, \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\
  \uAE30 \uC704\uD574 Date \uAC1D\uCCB4\uC5D0 \uC758\uC874\uD569\uB2C8\uB2E4. \uADF8\
  \uB7EC\uB098 JS/TS\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uB2E4\uB8E8\uB294 \uAC83\uC740\
  \ Date \uAC1D\uCCB4\uC758 \uD2B9\uC131 \uB54C\uBB38\uC5D0 \uC7A5\uD669\uD574\uC9C0\
  \uAC70\uB098 \uC815\uD655\uD558\uC9C0 \uC54A\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uB2E4\uC74C\uC740 \uAE30\uBCF8\uC801\uC778 \uC608\uC640 \uBCF4\uB2E4 \uACAC\uACE0\
  \uD55C\u2026"
lastmod: '2024-03-13T22:44:54.867061-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uB294 JavaScript\uC758 \uC288\uD37C\uC14B\uC73C\uB85C\uC11C,\
  \ \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uAE30 \uC704\
  \uD574 Date \uAC1D\uCCB4\uC5D0 \uC758\uC874\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
TypeScript는 JavaScript의 슈퍼셋으로서, 문자열에서 날짜를 파싱하기 위해 Date 객체에 의존합니다. 그러나 JS/TS에서 날짜를 다루는 것은 Date 객체의 특성 때문에 장황해지거나 정확하지 않을 수 있습니다. 다음은 기본적인 예와 보다 견고한 해결책을 위한 인기 있는 라이브러리인 `date-fns`를 사용하는 접근 방식입니다.

### JavaScript의 Date 객체 사용하기
```typescript
// Date 생성자를 사용한 기본 파싱
const dateFromString = new Date("2023-04-21T15:00:00Z");
console.log(dateFromString.toString()); 
// GMT 출력: "Fri Apr 21 2023 15:00:00 GMT+0000 (협정 세계시)"
```

이 방법은 ISO 형식 문자열과 다른 일부 날짜 형식에 대해 작동하지만, 브라우저와 지역에 따라 모호한 형식에 대해 일관성 없는 결과를 낳을 수 있습니다.

### date-fns 사용하기
`date-fns` 라이브러리는 날짜를 처리하는데 있어 직관적이고 일관된 방식을 제공합니다. 모듈식 라이브러리로 필요한 부분만 포함할 수 있어 번들 크기를 줄일 수 있습니다.

먼저, `date-fns`를 설치하세요:

```sh
npm install date-fns
```

그리고 나서, 날짜 문자열을 파싱하는 데 사용하세요:

```typescript
import { parseISO, format } from 'date-fns';

// ISO 문자열 파싱하기
const dateString = "2023-04-21T15:00:00Z";
const parsedDate = parseISO(dateString);

// 날짜 형식화하기 (예: 사람이 읽을 수 있는 형태로)
console.log(format(parsedDate, "PPPpp")); 
// 출력: "2023년 4월 21일 오후 3시 00분" (출력 결과는 로케일에 따라 달라질 수 있음)
```

`date-fns`는 다양한 형식과 지역을 지원하여, 다른 사용자 지역에서 정밀한 날짜 파싱과 형식화가 필요한 애플리케이션에 강력한 선택이 됩니다.
