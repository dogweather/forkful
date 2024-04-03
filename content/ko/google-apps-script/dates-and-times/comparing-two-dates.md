---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:26.644212-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C \uB0A0\uC9DC\uB294 JavaScript\uC758\
  \ Date \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBE44\uAD50\uB429\uB2C8\uB2E4\
  . \uC774\uB97C \uD1B5\uD574 \uB450 \uB0A0\uC9DC \uC911 \uC5B4\uB290 \uAC83\uC774\
  \ \uB354 \uC774\uC804\uC774\uAC70\uB098, \uB354 \uB098\uC911\uC774\uAC70\uB098,\
  \ \uB610\uB294 \uAC19\uC740\uC9C0\uB97C \uD3C9\uAC00\uD558\uAE30 \uC704\uD55C \uD45C\
  \uC900 \uBC29\uBC95\uC744 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uAE30\
  \uBCF8\uC801\uC778 \uC811\uADFC \uBC29\uBC95\uC740 \uB2E4\uC74C\uACFC \uAC19\uC2B5\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.551482-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uB0A0\uC9DC\uB294 JavaScript\uC758 Date\
  \ \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uBE44\uAD50\uB429\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## 방법:
Google Apps Script에서 날짜는 JavaScript의 Date 객체를 사용하여 비교됩니다. 이를 통해 두 날짜 중 어느 것이 더 이전이거나, 더 나중이거나, 또는 같은지를 평가하기 위한 표준 방법을 사용할 수 있습니다. 기본적인 접근 방법은 다음과 같습니다:

```javascript
function compareDates() {
  var date1 = new Date('2023-04-01T00:00:00');
  var date2 = new Date('2023-04-15T00:00:00');

  // 날짜 비교
  if (date1 < date2) {
    Logger.log('Date1이 Date2보다 이전입니다');
  } else if (date1 > date2) {
    Logger.log('Date1이 Date2보다 이후입니다');
  } else {
    Logger.log('두 날짜는 같습니다');
  }
}

// 샘플 출력:
// Date1이 Date2보다 이전입니다
```

보다 상세한 비교(예: 두 날짜 사이의 일 수)를 위해, 한 날짜에서 다른 날짜를 빼면 그 차이가 밀리초 단위로 반환됩니다:

```javascript
function daysBetweenDates() {
  var date1 = new Date('2023-04-01');
  var date2 = new Date('2023-04-15');
  
  var difference = date2 - date1;
  
  var days = difference / (1000 * 60 * 60 * 24); // 밀리초를 일로 변환
  Logger.log(days + ' 일 사이의 날짜들');
}

// 샘플 출력:
// 날짜들 사이에 14일
```

## 심층 탐구
Google Apps Script는 날짜 비교를 위해 JavaScript Date 객체의 핵심 원리를 활용합니다. 이는 언어가 처음 만들어진 이래로 기본적인 측면이었습니다. 유닉스 시대(1970년 1월 1일) 이후 밀리초를 비교 값으로 사용하는 것은 날짜 간의 차이 또는 유사성을 결정하는 데 높은 정밀도를 제공합니다.

Google Apps Script의 범위 내 대부분의 사용 사례에 대해 이 접근 방식이 효과적이긴 하지만, 시간대 수정 및 윤년 계산과 같은 날짜 작업은 때때로 혼란을 초래할 수 있습니다. 다른 프로그래밍 배경(예: `datetime` 및 `dateutil` 모듈이 날짜 처리를 보다 섬세하게 다루는 Python)의 개발자들은 JavaScript Date 객체가 기능면에서 부족하다고 느낄 수 있습니다.

단순 비교를 넘어 복잡한 날짜 처리 및 조작을 위해, `Moment.js`와 같은 라이브러리(외부 API를 통해 Google Apps Script 내에서도 사용할 수 있음)는 이러한 단점을 해결하는 풍부한 기능 세트를 제공합니다. 그러나, JavaScript의 네이티브 Date 객체는 특히 Google Apps Script와 Google 애플리케이션 모음과의 통합 맥락에서 대부분의 날짜 비교 작업에서 계속해서 신뢰할 수 있는 도구로서 작동합니다.
