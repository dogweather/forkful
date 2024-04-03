---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:34.089581-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\
  \uB294 \uAC83\uC740 \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uD14D\uC2A4\uD2B8\
  \ \uD615\uD0DC\uC758 \uB0A0\uC9DC \uD45C\uD604\uC744 \uC790\uBC14\uC2A4\uD06C\uB9BD\
  \uD2B8 `Date` \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uC5EC \uB0A0\uC9DC \uC870\uC791\
  , \uBE44\uAD50, \uD3EC\uB9E4\uD305 \uC791\uC5C5\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\
  \uAE30 \uC704\uD568\uC785\uB2C8\uB2E4. \uC774 \uACFC\uC815\uC740 \uC0AC\uC6A9\uC790\
  \ \uC785\uB825\uC744 \uCC98\uB9AC\uD558\uAC70\uB098, \uB370\uC774\uD130\uBCA0\uC774\
  \uC2A4\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD558\uAC70\uB098, \uBB38\
  \uC790\uC5F4 \uD615\uC2DD\uC73C\uB85C \uB0A0\uC9DC\uB97C\u2026"
lastmod: '2024-03-13T22:44:55.804596-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uB294\
  \ \uAC83\uC740 \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uD14D\uC2A4\uD2B8 \uD615\
  \uD0DC\uC758 \uB0A0\uC9DC \uD45C\uD604\uC744 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \ `Date` \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD558\uC5EC \uB0A0\uC9DC \uC870\uC791,\
  \ \uBE44\uAD50, \uD3EC\uB9E4\uD305 \uC791\uC5C5\uC744 \uC6A9\uC774\uD558\uAC8C \uD558\
  \uAE30 \uC704\uD568\uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 무엇 & 왜?
문자열에서 날짜를 파싱하는 것은 프로그래머들이 텍스트 형태의 날짜 표현을 자바스크립트 `Date` 객체로 변환하여 날짜 조작, 비교, 포매팅 작업을 용이하게 하기 위함입니다. 이 과정은 사용자 입력을 처리하거나, 데이터베이스에서 데이터를 처리하거나, 문자열 형식으로 날짜를 통신하는 API와 작업할 때 필수적입니다.

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
