---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:26.644212-07:00
description: "Google Apps Script\uC5D0\uC11C \uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\
  \uD558\uB294 \uAC83\uC740 \uC2A4\uCF00\uC904\uB9C1, \uD0C0\uC784\uB77C\uC778, \uB610\
  \uB294 \uB0A0\uC9DC \uAD00\uB828 \uB370\uC774\uD130\uB97C \uB2E4\uB8E8\uB294 \uAC1C\
  \uBC1C\uC790\uC5D0\uAC8C \uD544\uC218\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  . \uB0A0\uC9DC\uB97C \uC815\uD655\uD558\uAC8C \uBE44\uAD50\uD558\uB294 \uBC29\uBC95\
  \uC744 \uC774\uD574\uD558\uB294 \uAC83\uC740 \uB9C8\uAC10\uC77C, \uC774\uBCA4\uD2B8\
  \ \uACC4\uD68D, \uB610\uB294 \uCF58\uD150\uCE20 \uC2A4\uCF00\uC904\uB9C1\uACFC \uAC19\
  \uC740 \uAE30\uB2A5\uC744 \uD6A8\uACFC\uC801\uC73C\uB85C \uAD6C\uD604\uD560 \uC218\
  \u2026"
lastmod: 2024-02-19 22:05:13.493390
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uB450 \uB0A0\uC9DC\uB97C \uBE44\uAD50\uD558\
  \uB294 \uAC83\uC740 \uC2A4\uCF00\uC904\uB9C1, \uD0C0\uC784\uB77C\uC778, \uB610\uB294\
  \ \uB0A0\uC9DC \uAD00\uB828 \uB370\uC774\uD130\uB97C \uB2E4\uB8E8\uB294 \uAC1C\uBC1C\
  \uC790\uC5D0\uAC8C \uD544\uC218\uC801\uC778 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uB0A0\
  \uC9DC\uB97C \uC815\uD655\uD558\uAC8C \uBE44\uAD50\uD558\uB294 \uBC29\uBC95\uC744\
  \ \uC774\uD574\uD558\uB294 \uAC83\uC740 \uB9C8\uAC10\uC77C, \uC774\uBCA4\uD2B8 \uACC4\
  \uD68D, \uB610\uB294 \uCF58\uD150\uCE20 \uC2A4\uCF00\uC904\uB9C1\uACFC \uAC19\uC740\
  \ \uAE30\uB2A5\uC744 \uD6A8\uACFC\uC801\uC73C\uB85C \uAD6C\uD604\uD560 \uC218\u2026"
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Google Apps Script에서 두 날짜를 비교하는 것은 스케줄링, 타임라인, 또는 날짜 관련 데이터를 다루는 개발자에게 필수적인 작업입니다. 날짜를 정확하게 비교하는 방법을 이해하는 것은 마감일, 이벤트 계획, 또는 콘텐츠 스케줄링과 같은 기능을 효과적으로 구현할 수 있게 해줍니다.

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
