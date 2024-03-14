---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:25.448207-07:00
description: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uB294 \uAC83\
  \uC740 Google Apps Script \uCF54\uB4DC\uB97C \uB17C\uB9AC\uC801\uC778 \uC138\uADF8\
  \uBA3C\uD2B8\uB85C \uBD84\uB9AC\uD558\uC5EC \uAC01\uAC01 \uD2B9\uC815 \uC791\uC5C5\
  \uC744 \uC218\uD589\uD558\uB294 \uB3C5\uB9BD\uB41C \uBE14\uB85D\uC73C\uB85C \uAD6C\
  \uC870\uD654\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uC758 \uAC00\uB3C5\uC131, \uC720\uC9C0\uBCF4\
  \uC218\uC131 \uBC0F \uC7AC\uC0AC\uC6A9\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uACE0\
  \ \uBCF5\uC7A1\uD55C \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB354 \uC27D\uAC8C \uC774\uD574\
  \uD558\uACE0\u2026"
lastmod: '2024-03-13T22:44:54.539581-06:00'
model: gpt-4-0125-preview
summary: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uB294 \uAC83\uC740\
  \ Google Apps Script \uCF54\uB4DC\uB97C \uB17C\uB9AC\uC801\uC778 \uC138\uADF8\uBA3C\
  \uD2B8\uB85C \uBD84\uB9AC\uD558\uC5EC \uAC01\uAC01 \uD2B9\uC815 \uC791\uC5C5\uC744\
  \ \uC218\uD589\uD558\uB294 \uB3C5\uB9BD\uB41C \uBE14\uB85D\uC73C\uB85C \uAD6C\uC870\
  \uD654\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uCF54\uB4DC\uC758 \uAC00\uB3C5\uC131, \uC720\uC9C0\uBCF4\uC218\
  \uC131 \uBC0F \uC7AC\uC0AC\uC6A9\uC131\uC744 \uD5A5\uC0C1\uC2DC\uD0A4\uACE0 \uBCF5\
  \uC7A1\uD55C \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB354 \uC27D\uAC8C \uC774\uD574\uD558\
  \uACE0\u2026"
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?

코드를 함수로 구성하는 것은 Google Apps Script 코드를 논리적인 세그먼트로 분리하여 각각 특정 작업을 수행하는 독립된 블록으로 구조화하는 것을 말합니다. 프로그래머들은 코드의 가독성, 유지보수성 및 재사용성을 향상시키고 복잡한 스크립트를 더 쉽게 이해하고 디버깅할 수 있도록 이런 방식을 사용합니다.

## 방법:

Google Apps Script는 JavaScript를 기반으로 하며, `function` 키워드를 사용하여 함수를 정의합니다. 이어서 고유 함수 이름, 매개변수를 포함할 수 있는 괄호 `()`와 함수의 코드 블록을 포함하는 중괄호 `{}`가 따릅니다. 여기 간단한 예가 있습니다:

```javascript
function greetUser() {
  var user = Session.getActiveUser().getEmail();
  Logger.log('Hello, ' + user + '!');
}

greetUser();
```

샘플 출력:

```
Hello, someone@example.com!
```

이제 Google Sheets와 관련된 더 실용적인 예를 살펴보겠습니다. 여기서 기능을 두 개의 함수로 분리합니다: 하나는 시트를 설정하고 다른 하나는 데이터로 시트를 채웁니다.

```javascript
function setupSheet() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var sheet = ss.getSheets()[0];
  sheet.setName('Sales Data');
  sheet.appendRow(['Item', 'Quantity', 'Price']);
}

function populateSheet(data) {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Sales Data');
  data.forEach(function(row) {
    sheet.appendRow(row);
  });
}

// 데이터 배열 초기화
var salesData = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// 함수 실행
setupSheet();
populateSheet(salesData);
```

이 예에서, `setupSheet`은 시트를 준비하고, `populateSheet`은 판매 데이터 배열을 사용하여 시트를 채웁니다. 이렇게 관심사를 분리하면 코드가 더 깨끗하고 변경에 더 적응하기 쉬워집니다.

## 심층 탐구

코드를 함수로 나누는 개념은 Google Apps Script에만 국한되지 않으며, 거의 모든 프로그래밍 언어에서 권장하는 기본적인 프로그래밍 방식입니다. 역사적으로 함수는 입력을 출력으로 매핑하는 수학적 개념에서 발전했으며, 이는 구조적 프로그래밍에서 핵심 원칙이 되었습니다. 이 접근법은 모듈성과 코드 재사용을 촉진하며, 스크립트의 개별 부분을 테스트하는 명확한 경로를 제공합니다.

JavaScript 기반이며 Google Apps Script는 JavaScript의 일급 함수의 큰 이점을 누립니다. 이를 통해 함수를 인수로 전달하고 다른 함수에서 반환하며 변수에 할당할 수 있습니다. 이 기능은 콜백과 함수형 프로그래밍과 같은 고급 패턴을 가능하게 하지만, Google Apps Script에서의 단순한 자동화 작업에는 불필요한 복잡성을 도입할 수 있습니다.

보다 큰 프로젝트 또는 더 복잡한 애플리케이션의 경우, 개발자들은 JavaScript의 새로운 기능인 화살표 함수, 비동기 작업을 위한 async/await 및 정적 타이핑을 위한 TypeScript를 탐색할 수 있습니다. 특히 TypeScript는 Google Apps Script로 컴파일될 수 있으며, 더 견고한 타입 검사 및 고급 객체 지향 기능을 추구하는 개발자들에게 한 가지 방법을 제공합니다.

그러나 Google Apps 스위트 내에서 대부분의 스크립팅 필요에 대해 간단하고 잘 구성된 함수를 사용하는 것은 견고한 기반을 제공합니다. 효율성을 위해 고급 기능을 활용하는 것과 유지보수 및 가독성의 용이성을 위해 단순성을 유지하는 것 사이의 균형을 맞추는 것이 항상 중요합니다.
