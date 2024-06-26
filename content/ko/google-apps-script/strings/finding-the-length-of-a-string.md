---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:03.327089-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C\uB294 \uC790\uBC14\uC2A4\
  \uD06C\uB9BD\uD2B8\uC640 \uC720\uC0AC\uD558\uAC8C `.length` \uC18D\uC131\uC744 \uC0AC\
  \uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uC744 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4. \uC774 \uC18D\uC131\uC740 \uACF5\uBC31\uACFC \uD2B9\uC218\
  \ \uBB38\uC790\uB97C \uD3EC\uD568\uD55C \uBB38\uC790\uC5F4 \uB0B4\uC758 \uBB38\uC790\
  \ \uC218\uB97C \uBC18\uD658\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uBA87 \uAC00\uC9C0\
  \ \uC608\uC2DC\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.513776-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C\uB294 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \uC640 \uC720\uC0AC\uD558\uAC8C `.length` \uC18D\uC131\uC744 \uC0AC\uC6A9\uD558\uC5EC\
  \ \uBB38\uC790\uC5F4\uC758 \uAE38\uC774\uB97C \uCC3E\uC744 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4\uC758 \uAE38\uC774 \uCC3E\uAE30"
weight: 7
---

## 방법:
Google Apps Script에서는 자바스크립트와 유사하게 `.length` 속성을 사용하여 문자열의 길이를 찾을 수 있습니다. 이 속성은 공백과 특수 문자를 포함한 문자열 내의 문자 수를 반환합니다. 다음은 몇 가지 예시입니다:

```javascript
// 문자열 정의
var text = "Hello, World!";
// 문자열의 길이 찾기
var length = text.length;
// 길이 로깅
Logger.log(length); // 출력: 13
```

Google 양식이나 시트에서 사용자 입력을 다루는 시나리오에서 문자열 길이를 찾는 것은 데이터 검증에 도움이 됩니다:

```javascript
// Google 시트에서 사용자의 샘플 문자열 입력
var userEntry = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet().getRange("A1").getValue();
// 입력의 길이 계산 및 로깅
Logger.log(userEntry.length); // 출력은 셀 A1의 내용에 따라 다름
```

조건을 포함한 실용적인 예를 추가해 봅시다. 입력이 특정 길이를 초과하는 경우, 오류나 경고를 발생시키고 싶을 수 있습니다:

```javascript
var comment = "This is a sample comment that is too long for our database.";
if(comment.length > 50) {
  Logger.log("Error: Your comment should not exceed 50 characters.");
} else {
  Logger.log("Thank you for your submission.");
}
// 출력: Error: Your comment should not exceed 50 characters.
```

## 심층 탐구
Google Apps Script는 JavaScript에 기반을 둔 컨텍스트에서 `.length` 속성은 ECMAScript 표준에서 나온 것으로, JavaScript의 규격을 규정합니다. `.length` 속성은 JavaScript가 초기 단계부터 있었던 것으로, 문자열의 크기를 평가하는 간단한 방법을 제공합니다.

한 가지 주목할 점은 Google Apps Script가 브라우저가 아닌 Google의 서버에서 실행된다는 것입니다. 이는 Google 시트나 문서에서 검색된 대규모 데이터셋과 같이 문자열과 그 길이를 다룰 때, 네트워크 지연 및 스크립트의 실행 제한으로 인해 실행 시간이 영향을 받을 수 있다는 의미입니다.

`.length`는 문자열의 길이를 찾는 간단하고 널리 사용되는 방법이지만, 멀티바이트 문자를 다루거나 특정 유형의 문자를 필터링해야 할 때 문자를 반복하며 문자 수를 세거나 정규 표현식과 같은 대안적 전략이 필요할 수 있습니다. 그러나 Google Apps Script 내에서 대부분의 실용적인 목적에 대해, `.length`는 문자열 길이를 결정하는 믿을 수 있는 효율적인 방법을 제공합니다.

특히 Google Apps Script에서는 실행 중인 코드의 컨텍스트를 항상 기억하세요. 성능 및 실행 제한은 문자열 처리 절차를 최적화하는 방향으로 이끌 수 있으며, 이는 문자열의 길이를 결정하는 방법을 포함합니다.
