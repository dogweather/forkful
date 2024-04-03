---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:34.089581-07:00
description: "\uBC29\uBC95: \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uB294 \uB0A0\uC9DC\
  \ \uBB38\uC790\uC5F4\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 \uAE30\uBCF8\uC801\
  \uC73C\uB85C `Date.parse()` \uBA54\uC18C\uB4DC\uC640 `Date` \uC0DD\uC131\uC790\uB97C\
  \ \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD558\uC9C0\uB9CC, \uC774\uB7F0 \uC811\uADFC\uBC95\
  \uB4E4\uC740 \uD2B9\uD788 \uBE44\uD45C\uC900 \uB0A0\uC9DC \uD615\uC2DD\uACFC \uAD00\
  \uB828\uD558\uC5EC, \uB2E4\uB978 \uBE0C\uB77C\uC6B0\uC800\uB4E4 \uAC04\uC758 \uD55C\
  \uACC4\uC810\uACFC \uC77C\uAD00\uC131\uC774 \uC5C6\uB294 \uBB38\uC81C\uAC00 \uC788\
  \uC2B5\uB2C8\uB2E4. \uC774\uB7EC\uD55C \uBB38\uC81C\uB4E4\uC744\u2026"
lastmod: '2024-03-13T22:44:55.804596-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uB294 \uB0A0\uC9DC \uBB38\uC790\uC5F4\
  \uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD574 \uAE30\uBCF8\uC801\uC73C\uB85C `Date.parse()`\
  \ \uBA54\uC18C\uB4DC\uC640 `Date` \uC0DD\uC131\uC790\uB97C \uC81C\uACF5\uD569\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
자바스크립트는 날짜 문자열을 파싱하기 위해 기본적으로 `Date.parse()` 메소드와 `Date` 생성자를 제공합니다. 하지만, 이런 접근법들은 특히 비표준 날짜 형식과 관련하여, 다른 브라우저들 간의 한계점과 일관성이 없는 문제가 있습니다. 이러한 문제들을 해결하기 위해, `Moment.js`와 `date-fns` 같은 타사 라이브러리들이 그들의 견고함과 사용의 용이성으로 인해 인기를 얻고 있습니다.

### 자바스크립트 네이티브 사용하기:
```javascript
const dateString = "2023-04-30T14:55:00";
const dateObj = new Date(dateString);

console.log(dateObj);  // 출력: Sun Apr 30 2023 14:55:00 GMT+0000 (협정 세계시)
```

### Moment.js 사용하기:
먼저, npm을 통해 Moment.js를 설치하거나 프로젝트에 포함시킵니다. 그런 다음:
```javascript
const moment = require('moment');

const dateString = "2023-04-30T14:55:00";
const dateObj = moment(dateString);

console.log(dateObj.toString());  // 출력: Sun Apr 30 2023 14:55:00 GMT+0000
```

### date-fns 사용하기:
`date-fns`를 프로젝트에 추가한 후, 다음과 같이 날짜 문자열을 파싱하세요:
```javascript
const { parseISO } = require('date-fns');

const dateString = "2023-04-30T14:55:00";
const dateObj = parseISO(dateString);

console.log(dateObj);  // 출력: 2023-04-30T14:55:00.000Z
```

`Moment.js`와 `date-fns`는 다양한 형식과 로케일을 처리하는 것을 포함하여, 더 포괄적인 파싱 기능을 제공하여 복잡한 애플리케이션에 선호됩니다.
