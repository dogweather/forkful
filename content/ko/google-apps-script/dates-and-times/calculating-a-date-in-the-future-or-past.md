---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:25.620325-07:00
description: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC\uB97C \uACC4\
  \uC0B0\uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC \uAC1D\uCCB4\uB97C \uC870\uC791\uD558\
  \uC5EC \uD604\uC7AC \uB0A0\uC9DC\uB97C \uCD08\uC6D4\uD558\uAC70\uB098 \uC774\uC804\
  \uD558\uB294 \uB0A0\uC9DC\uB97C \uCC3E\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC54C\uB9BC \uC124\uC815, \uB9CC\
  \uB8CC \uB0A0\uC9DC \uC9C0\uC815\uBD80\uD130 \uC2DC\uAC04 \uAE30\uBC18 \uB370\uC774\
  \uD130 \uCD94\uC138 \uBD84\uC11D\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uB2E4\uC591\
  \uD55C \uC791\uC5C5\uC744 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:54.553120-06:00'
model: gpt-4-0125-preview
summary: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC\uB97C \uACC4\uC0B0\
  \uD558\uB294 \uAC83\uC740 \uB0A0\uC9DC \uAC1D\uCCB4\uB97C \uC870\uC791\uD558\uC5EC\
  \ \uD604\uC7AC \uB0A0\uC9DC\uB97C \uCD08\uC6D4\uD558\uAC70\uB098 \uC774\uC804\uD558\
  \uB294 \uB0A0\uC9DC\uB97C \uCC3E\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC54C\uB9BC \uC124\uC815, \uB9CC\uB8CC\
  \ \uB0A0\uC9DC \uC9C0\uC815\uBD80\uD130 \uC2DC\uAC04 \uAE30\uBC18 \uB370\uC774\uD130\
  \ \uCD94\uC138 \uBD84\uC11D\uC5D0 \uC774\uB974\uAE30\uAE4C\uC9C0 \uB2E4\uC591\uD55C\
  \ \uC791\uC5C5\uC744 \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
title: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\
  \uAE30"
weight: 26
---

## 무엇인가 & 왜인가?

미래 또는 과거의 날짜를 계산하는 것은 날짜 객체를 조작하여 현재 날짜를 초월하거나 이전하는 날짜를 찾는 것에 관한 것입니다. 프로그래머는 알림 설정, 만료 날짜 지정부터 시간 기반 데이터 추세 분석에 이르기까지 다양한 작업을 위해 이를 수행합니다.

## 방법:

JavaScript 기반인 Google Apps Script에서는 `Date` 객체를 사용하여 날짜를 조작할 수 있습니다. 미래와 과거의 날짜를 계산하는 방법은 다음과 같습니다:

### 미래 날짜 계산

미래 날짜를 계산하기 위해, 현재 날짜에 대한 날짜 객체를 생성한 다음 원하는 일수(또는 기타 시간 단위)를 추가합니다.

```javascript
// 현재 날짜
var today = new Date();

// 미래의 날짜 10일 계산
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("미래 날짜: " + futureDate.toDateString());
```

### 과거 날짜 계산

마찬가지로, 과거의 날짜를 찾기 위해 현재 날짜에서 일수를 빼면 됩니다.

```javascript
// 현재 날짜
var today = new Date();

// 과거의 날짜 10일 계산
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("과거 날짜: " + pastDate.toDateString());
```

### 샘플 출력

이는 다음과 같은 것을 출력할 것입니다 (오늘 날짜가 2023년 4월 15일이라고 가정하면):

```
미래 날짜: 화 4월 25 2023
과거 날짜: 수 4월 05 2023
```

JavaScript (그리고 Google Apps Script에서의) Date 객체는 날짜를 추가하거나 빼면서 자동으로 월과 년을 조정한다는 것을 기억하세요.

## 심층 탐구

`Date` 객체를 사용한 날짜 조작은 초기 JavaScript 구현에서 비롯되었습니다. 시간이 지나면서, 이 접근 방식은 개발자가 외부 라이브러리가 필요 없이 날짜를 관리할 수 있는 직관적인 방법을 제공하는 것으로 일반적으로 일관되게 유지되었습니다. 그러나 타임존 조정과 같은 더 복잡한 연산이나 광범위한 날짜 기반 데이터를 다룰 때는 `Moment.js` 또는 더 현대적인 `Luxon`과 같은 라이브러리가 더 많은 기능과 쉬운 처리를 제공할 수 있습니다.

특히 Google Apps Script에서는, `Date` 객체의 직접적인 가용성과 간단함에도 불구하고, 스크립트 성능과 실행 시간에 날짜 계산이 미칠 수 있는 영향을 염두에 두는 것이 중요합니다. 특히 시간 기반 트리거나 광범위한 스프레드시트 조작에서 그렇습니다. 또한 Google Apps Script는 Google 시트나 캘린더 등 자체 생태계 내에서 날짜를 처리하기 위한 기본 제공 메소드를 제공하지만, 외부 라이브러리를 통합하거나 Google의 고급 서비스를 활용하는 것이 때때로 복잡한 시나리오에 대한 더 강력한 해결책을 제공할 수 있습니다.

따라서, 네이티브 JavaScript `Date` 객체 방법론이 일반적으로 단순한 계산에는 충분하지만, 더 미묘한 요구에 대한 기능을 향상시키기 위해서는 외부 라이브러리나 서비스를 탐색하는 것이 유용할 수 있습니다.
