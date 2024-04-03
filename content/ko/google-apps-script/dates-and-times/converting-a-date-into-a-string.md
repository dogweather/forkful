---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:21.115647-07:00
description: "\uBC29\uBC95: Google Apps \uC2A4\uD06C\uB9BD\uD2B8\uB294 JavaScript\uB97C\
  \ \uAE30\uBC18\uC73C\uB85C \uD558\uAE30 \uB54C\uBB38\uC5D0, \uB0A0\uC9DC\uB97C \uBB38\
  \uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30 \uC704\uD55C \uC5EC\uB7EC \uBC29\uBC95\
  \uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uC544\uB798\uB294 \uB2E4\uC591\uD55C \uC811\
  \uADFC\uBC95\uC744 \uBCF4\uC5EC\uC8FC\uB294 \uC608\uC2DC\uB4E4\uC785\uB2C8\uB2E4\
  : #."
lastmod: '2024-03-13T22:44:54.549853-06:00'
model: gpt-4-0125-preview
summary: "Google Apps \uC2A4\uD06C\uB9BD\uD2B8\uB294 JavaScript\uB97C \uAE30\uBC18\
  \uC73C\uB85C \uD558\uAE30 \uB54C\uBB38\uC5D0, \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\
  \uB85C \uBCC0\uD658\uD558\uAE30 \uC704\uD55C \uC5EC\uB7EC \uBC29\uBC95\uC744 \uC81C\
  \uACF5\uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## 방법:
Google Apps 스크립트는 JavaScript를 기반으로 하기 때문에, 날짜를 문자열로 변환하기 위한 여러 방법을 제공합니다. 아래는 다양한 접근법을 보여주는 예시들입니다:

### `toString()` 메소드 사용하기:
가장 간단한 방법은 `toString()` 메소드를 사용하는 것이며, 이는 날짜 객체를 기본 형식의 문자열로 변환합니다.

```javascript
var date = new Date();  // 새로운 날짜 객체 생성
var dateString = date.toString();
Logger.log(dateString); // 출력: "Wed Apr 05 2023 12:34:56 GMT-0700 (Pacific Daylight Time)"
```

### `toDateString()` 메소드 사용하기:
시간 정보 없이 읽기 쉬운 형식으로 날짜 부분만 얻으려면 `toDateString()`을 사용할 수 있습니다.

```javascript
var date = new Date();
var dateString = date.toDateString();
Logger.log(dateString); // 출력: "Wed Apr 05 2023"
```

### 사용자 정의 형식을 위한 `Utilities.formatDate()` 사용하기:
형식을 더욱 제어하고 싶다면, Google Apps 스크립트는 `Utilities.formatDate()`를 제공합니다. 이 메소드는 세 가지 매개변수를 요구합니다: 날짜 객체, 시간대, 형식 문자열.

```javascript
var date = new Date();
var timeZone = Session.getScriptTimeZone();
var formattedDate = Utilities.formatDate(date, timeZone, "YYYY-MM-dd");
Logger.log(formattedDate); // 출력: "2023-04-05"
```

이 메소드는 지역 특화 형식이나 특정 응용 프로그램 요구사항에 적합한 날짜를 생성하는데 특히 유용합니다.

## 심층 분석
날짜를 문자열로 변환하는 필요성은 Google Apps 스크립트에만 국한되지 않으며, 모든 프로그래밍 언어에서 널리 볼 수 있습니다. 그러나 JavaScript로부터 물려받은 Google Apps 스크립트의 접근 방식은 웹 기반 스크립팅을 향해 맞춤화된 유연한 옵션들을 제공합니다. `Utilities.formatDate()`는 시간대와 함께 작업하는 복잡성을 인식하는데 눈에 띄며, 이는 종종 간과되는 도전입니다.

역사적으로, 날짜와 시간 처리는 주로 시간대와 형식의 차이로 인해 소프트웨어 개발에서 버그와 복잡성의 원천이 되었습니다. Google Apps 스크립트에서 `Utilities.formatDate()`의 도입은 특히 전 세계적으로 사용되는 Google의 제품군 맥락에서 날짜-시간 조작을 표준화하는 방향으로의 인정으로 볼 수 있습니다.

그러나 시간대, 지역, 형식을 정밀하게 제어해야 하는 경우, 특히 국제화된 응용 프로그램에서는 개발자가 `Moment.js`와 같은 외부 라이브러리를 활용하게 될 수 있습니다(번들 크기 문제와 현대 기능으로 인해 `Luxon`, `Day.js`, `date-fns`로의 선호도가 증가함에 따라). 이 접근법은 물론 외부 의존성 추가와 프로젝트 복잡성 증가의 절충안을 가져옵니다.

외부 라이브러리에 대한 잠재적인 요구에도 불구하고, `Utilities.formatDate()`와 네이티브 JavaScript 날짜 메소드는 대부분의 일반적인 사용 사례에 대한 강력한 솔루션을 제공합니다. 능숙한 개발자들은 내장 기능의 단순함과 편리함과 외부 라이브러리의 힘과 유연성 사이에서 균형을 유지할 것이며, 이는 그들 프로젝트의 특정 요구에 따라 다를 것입니다.
