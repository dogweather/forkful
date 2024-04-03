---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:27.273243-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB294\uAC00: Google Apps Script\uB294 \uC790\
  \uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC758 \uB0B4\uC7AC\uB41C \uAE30\uB2A5\uC744 \uD65C\
  \uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4 \uC870\uC791\uC744 \uC704\uD55C \uAC15\uB825\
  \uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uD328\uD134\uACFC \uC77C\
  \uCE58\uD558\uB294 \uBB38\uC790\uB97C \uC0AD\uC81C\uD558\uAE30 \uC704\uD574, \uC6B0\
  \uB9AC\uB294 \uD2B9\uC815 \uD328\uD134\uC744 \uAC80\uC0C9\uD558\uACE0, \uC6B0\uB9AC\
  \uC758 \uACBD\uC6B0\uC5D0\uB294 \uADF8\uAC83\uB4E4\uC744 \uC81C\uAC70\uD558\uB294\
  \ \uAC83\uC744 \uAC00\uB2A5\uD558\uAC8C \uD558\uB294 \uC815\uADDC\u2026"
lastmod: '2024-03-13T22:44:54.502478-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uB294 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC758 \uB0B4\
  \uC7AC\uB41C \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD558\uC5EC \uBB38\uC790\uC5F4 \uC870\
  \uC791\uC744 \uC704\uD55C \uAC15\uB825\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C\uD558\
  \uAE30"
weight: 5
---

## 어떻게 하는가:
Google Apps Script는 자바스크립트의 내재된 기능을 활용하여 문자열 조작을 위한 강력한 방법을 제공합니다. 패턴과 일치하는 문자를 삭제하기 위해, 우리는 특정 패턴을 검색하고, 우리의 경우에는 그것들을 제거하는 것을 가능하게 하는 정규 표현식(regex)을 사용합니다.

실용적인 예는 다음과 같습니다:

```javascript
function removeCharacters() {
  var originalString = "123-ABC-456-DEF";
  var pattern = /[^A-Z]+/g; // 대문자가 아닌 모든 것과 일치하는 Regex
  var cleanedString = originalString.replace(pattern, ""); // 일치하는 문자를 제거
  
  Logger.log("원본: " + originalString); // 원본: 123-ABC-456-DEF
  Logger.log("정제됨: " + cleanedString); // 정제됨: ABCDEF
}
```

위 스크립트는 대문자가 아닌 모든 문자와 일치하는 패턴을 정의하고 문자열에서 그것들을 제거합니다. 이것은 혼합 형식 입력에서 특정 유형의 데이터(예: 문자만)를 추출해야 할 때 특히 유용합니다.

## 심층 분석:
정규 표현식을 사용한 문자열 조작은 컴퓨팅의 초기 단계로 거슬러 올라가며, Google Apps Script를 포함한 다양한 프로그래밍 환경에서 패턴 인식을 위한 강력한 도구로 발전했습니다. 정규 표현식은 패턴 매칭과 문자 삭제에서 비할 데 없는 유연성과 효율성을 제공하지만, 그 적용을 신중하게 접근하는 것이 중요합니다. 오용이나 지나치게 복잡한 패턴은 성능 병목 현상이나 읽기 힘든 코드로 이어질 수 있습니다.

Google Apps Script 내에서, 이 구현은 자바스크립트의 `String.replace()` 메소드를 활용하여, Apps Script에 새로운 사용자이지만 자바스크립트에 익숙한 사람들까지도 접근할 수 있게 합니다. 그러나, 특히 큰 데이터셋이나 복잡한 Google 시트를 다루는 경우에는 실행 시간 제한을 피하고 스크립트 효율성을 향상시키기 위해 대안적인 방법이나 데이터 전처리를 다루는 애드온을 고려하는 것이 유익할 수 있습니다.

정규 표현식이 패턴 기반 문자 삭제를 위한 강력한 방법으로 남아있음에도 불구하고, Google Apps Script의 내장된 문자열 및 배열 메소드를 더 단순한 작업을 위해 탐색하거나 보다 복잡한 시나리오를 위해 외부 라이브러리를 사용하는 것이 성능과 유지 보수성의 균형을 제공하는 더 최적화된 해결책이 될 수 있습니다.
