---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:52.401073-07:00
description: "\uBC29\uBC95: JavaScript\uB97C \uAE30\uBC18\uC73C\uB85C \uD558\uB294\
  \ Google Apps Script\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C\
  \ \uD30C\uC2F1\uD558\uB294 \uB370 \uC5EC\uB7EC \uC811\uADFC \uBC29\uC2DD\uC744 \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798\uB294 \uB124\uC774\uD2F0\
  \uBE0C JavaScript \uBA54\uC11C\uB4DC\uC640 Google Apps Script \uC720\uD2F8\uB9AC\
  \uD2F0\uB97C \uC0AC\uC6A9\uD55C \uC608\uC2DC\uC785\uB2C8\uB2E4. **`new\u2026"
lastmod: '2024-03-13T22:44:54.546563-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\uB97C \uAE30\uBC18\uC73C\uB85C \uD558\uB294 Google Apps Script\uC5D0\
  \uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD558\uB294\
  \ \uB370 \uC5EC\uB7EC \uC811\uADFC \uBC29\uC2DD\uC744 \uC0AC\uC6A9\uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
JavaScript를 기반으로 하는 Google Apps Script에서 문자열에서 날짜를 파싱하는 데 여러 접근 방식을 사용할 수 있습니다. 아래는 네이티브 JavaScript 메서드와 Google Apps Script 유틸리티를 사용한 예시입니다.

**`new Date()` 생성자 사용하기:**

Google Apps Script에서 문자열을 날짜로 파싱하는 가장 간단한 방법은 `Date` 객체의 생성자를 사용하는 것입니다. 그러나 이 방법은 날짜 문자열이 Date.parse() 메서드에서 인식하는 형식(예: YYYY-MM-DD)이어야 합니다.

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)를 로그에 기록
```

**`Utilities.parseDate()` 사용하기:**

보다 맞춤형 날짜 형식에 대한 유연성을 원할 경우, Google Apps Script는 `Utilities.parseDate()`를 제공합니다. 이 메서드는 날짜 형식, 시간대 및 지역을 지정할 수 있게 합니다.

```javascript
const dateString = '01-04-2023'; // DD-MM-YYYY
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // 스크립트의 시간대에 따라 Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)를 로그에 기록
```

주의: `Utilities.parseDate()`는 더 많은 제어를 제공하지만, 스크립트의 시간대에 따라 동작이 달라질 수 있으므로, 애플리케이션이 여러 지역의 날짜를 처리하는 경우 명시적으로 시간대를 지정하는 것이 중요합니다.

## 심화 탐구
프로그래밍 언어에서 날짜 파싱은 역사적으로 다양한 날짜 형식과 시간대의 복잡성으로 인한 도전 과제가 되어 왔습니다. JavaScript에서 주로 유래한 Google Apps Script의 접근 방식은 `Date` 객체와 더 다양하게 사용할 수 있는 `Utilities.parseDate()` 함수를 제공함으로써 이를 단순화하려고 시도합니다. 그러나 각 방법은 제한이 있습니다; 예를 들어, 문자열을 사용한 `Date` 생성자에 의존하는 것은 날짜 형식의 다른 환경에 대한 해석 차이로 인해 일관성이 없게 될 수 있습니다. 반면에, `Utilities.parseDate()`는 형식, 시간대, 및 지역에 대해 더 명확한 이해가 필요하며, 특정 필요에 대해 약간 더 복잡하지만 더 신뢰할 수 있게 합니다.

Moment.js(이제 새 프로젝트에는 Luxon을 권장함)와 같은 대체 라이브러리나 서비스는 더 풍부한 기능성과 더 나은 시간대 처리를 제공하여 많은 이러한 도전 과제를 해결합니다. 그러나 외부 라이브러리가 제한적인 Google Apps Script의 맥락에서는 내장 메서드를 효과적으로 이해하고 활용하는 것이 중요합니다. 다른 언어에서 온 프로그래머들은 Google Apps Script에서 날짜 처리의 미묘함을 독특하게 어려워할 수 있지만, 사용 가능한 도구에 대한 깊은 이해와 응용 프로그램의 글로벌 성격을 신중히 고려함으로써 견고한 날짜 파싱을 달성할 수 있습니다.
