---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:30.035530-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C \uB9AC\uD329\uD1A0\uB9C1\
  \uC774 \uC720\uC6A9\uD55C \uC77C\uBC18\uC801\uC778 \uC0C1\uD669\uC740 Google \uC2DC\
  \uD2B8 \uB610\uB294 \uBB38\uC11C\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uBC88\
  \uAC70\uB85C\uC6B4 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB2E8\uC21C\uD654\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uCC98\uC74C\uC5D0\uB294 \uBE60\uB978 \uACB0\uACFC\uB97C\
  \ \uC5BB\uAE30 \uC704\uD574 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uBE60\uB974\uACE0 \uC9C0\
  \uC800\uBD84\uD558\uAC8C \uC791\uC131\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC2DC\
  \uAC04\uC774 \uC9C0\uB098\uBA74\uC11C \uC2A4\uD06C\uB9BD\uD2B8\uAC00\u2026"
lastmod: '2024-03-13T22:44:54.544875-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uB9AC\uD329\uD1A0\uB9C1\uC774 \uC720\uC6A9\
  \uD55C \uC77C\uBC18\uC801\uC778 \uC0C1\uD669\uC740 Google \uC2DC\uD2B8 \uB610\uB294\
  \ \uBB38\uC11C\uC640 \uC0C1\uD638\uC791\uC6A9\uD558\uB294 \uBC88\uAC70\uB85C\uC6B4\
  \ \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB2E8\uC21C\uD654\uD558\uB294 \uAC83\uC785\uB2C8\
  \uB2E4."
title: "\uB9AC\uD329\uD1A0\uB9C1"
weight: 19
---

## 방법:
Google Apps Script에서 리팩토링이 유용한 일반적인 상황은 Google 시트 또는 문서와 상호작용하는 번거로운 스크립트를 단순화하는 것입니다. 처음에는 빠른 결과를 얻기 위해 스크립트를 빠르고 지저분하게 작성할 수 있습니다. 시간이 지나면서 스크립트가 커지면 다루기 힘들어집니다. 가독성과 효율성을 위해 리팩토링하는 예를 살펴보겠습니다.

**원본 스크립트:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

이 함수는 Google 스프레드시트의 각 시트 이름을 로그합니다. 작동은 잘 하지만, 구식 자바스크립트 관행을 사용하고 명확성이 부족합니다.

**리팩토링된 스크립트:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

리팩토링된 버전에서는 변경되지 않는 변수에 대해 `const`를 사용함으로써 의도를 더 명확히 하였습니다. 또한 배열을 반복하는 데 있어서 더 현대적이고 간결한 접근 방식인 `forEach` 메서드를 사용하여 가독성을 향상시켰습니다.

**샘플 출력 (두 스크립트 모두에 대하여):**

"비용"과 "수익"이라는 두 시트를 가진 Google 시트 문서를 가정하면, Logger에서의 출력은 다음과 같아 보일 것입니다:

```
[20-04-2023 10:00:00: INFO] 비용
[20-04-2023 10:00:01: INFO] 수익
```

리팩토링된 스크립트는 같은 결과를 달성하지만 더 깨끗하고 한눈에 이해하기 쉽습니다.

## 심화 탐구
Google Apps Script에서의 리팩토링은 더 넓은 소프트웨어 공학 실습에서 그 원칙을 부분적으로 상속합니다. 1990년대 후반, 특히 마틴 파울러의 기념비적인 책 "리팩토링: 기존 코드의 설계 개선"(1999)으로 인해 개념이 더 인식되고 체계화되었으며, 다양한 리팩토링 기술에 대한 포괄적인 안내서를 제공했습니다. 프로그래밍 언어의 문법적 및 기능적 차이로 인해 리팩토링의 구체적인 사항은 다양할 수 있지만, 핵심 목표는 외부 행동을 변경하지 않고 코드를 개선하는 것이 동일합니다.

Google Apps Script의 맥락에서 리팩토링하는 동안 고려해야 할 핵심 측면 중 하나는 Google이 부과한 서비스 할당량과 제한입니다. 효율적으로 리팩토링된 코드는 더 나은 가독성만이 아니라 이러한 제약 내에서 더 빠르고 신뢰성 있게 실행됩니다. 예를 들어, 배치 작업(`Range.setValues()`를 사용하여 한 번에 한 셀씩 값을 설정하는 것 대신)은 실행 시간과 할당량 소비를 상당히 줄일 수 있습니다.

하지만, 특정 복잡한 프로젝트에 대해 Google Apps Script는 이러한 제한 때문에 부족하다고 느껴질 수 있습니다. 이러한 경우, Google Cloud Functions 또는 Apps Script의 새로운 형제인 AppSheet와 같은 대안을 탐색하는 것이 더 나은 확장성과 기능을 제공할 수 있습니다.

결국, Google Apps Script 프로젝트를 유지하고 개선하는 데에 있어 리팩토링은 중요한 기술이지만, 환경의 제한을 이해하고 대안 솔루션을 고려하는 것이 효율적이고 견고하며 유지 가능한 코드를 제공하는 데 있어서 마찬가지로 중요합니다.
