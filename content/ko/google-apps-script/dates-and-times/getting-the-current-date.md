---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:43.564737-07:00
description: "Google Apps Script\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\
  \uC838\uC624\uB294 \uAC83\uC740 \uC2E4\uC2DC\uAC04 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\
  \uC744 \uAC00\uC838\uC624\uB294 \uC77C\uBC18\uC801\uC778 \uC791\uC5C5\uC73C\uB85C\
  , Google\uC758 \uC0DD\uD0DC\uACC4\uC5D0 \uC5F0\uACB0\uB41C \uC571\uC5D0\uC11C \uC790\
  \uB3D9\uD654 \uC791\uC5C5, \uB85C\uAE45, \uD0C0\uC784\uC2A4\uD0EC\uD551 \uB4F1\uC744\
  \ \uC704\uD574 \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC774 \uAE30\uB2A5\uC744 \uB3D9\uC801 \uCF58\uD150\uCE20 \uC0DD\uC131, \uB9C8\
  \uAC10\uC77C \uCD94\uC801,\u2026"
lastmod: '2024-03-13T22:44:54.548182-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\
  \uC624\uB294 \uAC83\uC740 \uC2E4\uC2DC\uAC04 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744\
  \ \uAC00\uC838\uC624\uB294 \uC77C\uBC18\uC801\uC778 \uC791\uC5C5\uC73C\uB85C, Google\uC758\
  \ \uC0DD\uD0DC\uACC4\uC5D0 \uC5F0\uACB0\uB41C \uC571\uC5D0\uC11C \uC790\uB3D9\uD654\
  \ \uC791\uC5C5, \uB85C\uAE45, \uD0C0\uC784\uC2A4\uD0EC\uD551 \uB4F1\uC744 \uC704\
  \uD574 \uC0AC\uC6A9\uB429\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uC5BB\uAE30"
weight: 29
---

## 방법:
JavaScript를 기반으로 하는 Google Apps Script는 현재 날짜를 가져오는 직관적인 방법을 제공합니다. `new Date()` 생성자를 사용하여 현재 날짜와 시간을 나타내는 새로운 날짜 객체를 생성할 수 있습니다. 다음은 이를 다양한 형식으로 조작하고 표시하는 방법입니다.

```javascript
function showCurrentDate() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // 스크립트의 시간대에서 현재 날짜와 시간을 로그합니다.
  
  // 날짜만 YYYY-MM-DD 형식으로 표시
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // 예시 출력: "2023-04-01"
  
  // 더 읽기 쉬운 형식으로 표시
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // 예시 출력: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

이 스니펫은 현재 날짜와 시간을 캡처하고 형식화하는 방법을 보여주며, Google Apps Script 내에서 다양한 프로그래밍 요구에 대한 다재다능함을 보여줍니다.

## 심층 분석
JavaScript가 `Date` 객체에 정착하기 전에는, 프로그래머들은 타임스탬프 정수 및 자체 제작 날짜 함수와 같은 표준화되지 않고 더 번거로운 수단을 사용하여 수동으로 시간과 날짜를 추적해야 했습니다. 이는 프로그래밍 환경마다 달라져 일관성 및 호환성 문제를 야기했습니다.

JavaScript 및 확장하여 Google Apps Script에서 `new Date()` 객체의 도입은 날짜 및 시간 연산을 표준화하고, 날짜 관련 작업에 필요한 코드 양을 줄여 더 직관적이고 편리하게 만들었습니다. Google Apps Script의 구현이 Google 제품군 내 많은 애플리케이션에 편리하고 충분하다는 점은 주목할 가치가 있지만, 복잡한 시간대 처리나 빠르게 움직이는 환경에서 정밀한 타임스탬프 로깅을 요구하는 시나리오를 모두 수용하지는 못할 수도 있습니다.

이러한 고급 사용 사례의 경우, 프로그래머들은 종종 Moment.js 또는 JavaScript의 date-fns와 같은 라이브러리로 전환합니다. Google Apps Script는 이러한 라이브러리를 기본적으로 지원하지 않지만, 개발자들은 사용 가능한 JavaScript Date 메소드를 사용하거나 HTML 서비스나 Apps Script의 URL Fetch 서비스를 통해 외부 라이브러리에 접근함으로써 일부 기능을 모방할 수 있습니다. 이러한 대안에도 불구하고, Google Apps Script의 기본 날짜 및 시간 함수의 단순함과 통합은 대부분의 Google 생태계 작업을 위한 주요 도구로 남아 있습니다.
