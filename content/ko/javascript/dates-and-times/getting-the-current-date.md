---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:08.289939-07:00
description: "\uC5B4\uB5BB\uAC8C: \uBC14\uB2D0\uB77C \uC790\uBC14\uC2A4\uD06C\uB9BD\
  \uD2B8\uC5D0\uC11C\uB294 `Date` \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB0A0\
  \uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8F9\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uD604\
  \uC7AC \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uC5BB\uB294 \uBC29\uBC95\uC785\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.806230-06:00'
model: gpt-4-0125-preview
summary: "\uBC14\uB2D0\uB77C \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C\uB294\
  \ `Date` \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uB0A0\uC9DC\uC640 \uC2DC\uAC04\
  \uC744 \uB2E4\uB8F9\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 어떻게:
바닐라 자바스크립트에서는 `Date` 객체를 사용하여 날짜와 시간을 다룹니다. 다음은 현재 날짜와 시간을 얻는 방법입니다:

```javascript
const currentDate = new Date();
console.log(currentDate); // 예시 출력: Fri Apr 14 2023 12:34:56 GMT+0100 (British Summer Time)
```

더 친숙하고 사용자 친화적인 형식으로 날짜만 표시하려면 `toLocaleDateString()`과 같은 메소드를 사용할 수 있습니다:

```javascript
console.log(currentDate.toLocaleDateString()); // 예시 출력: 4/14/2023
```

형식을 더 많이 제어하려면 *Moment.js* 또는 *date-fns*와 같은 서드파티 라이브러리를 사용하는 것이 매우 인기가 있지만, Moment.js는 현재 유지 관리 모드의 레거시 프로젝트로 간주된다는 것을 알고 있어야 합니다.

*Moment.js* 사용:

```javascript
const moment = require('moment'); // Node.js를 가정하거나 모듈 번들러를 사용하는 경우
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // 예시 출력: 2023-04-14
```

필요한 것만 가져오도록 모듈화를 강조하는 *date-fns*를 사용하면:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // 예시 출력: 2023-04-14
```

자바스크립트에서 날짜를 다루기 위한 각 접근 방식은 내장된 `Date` 객체부터 라이브러리를 통해 사용할 수 있는 더 정교한 서식 및 조작 기능에 이르기까지 다양한 편의성과 유연성 수준을 제공합니다.
