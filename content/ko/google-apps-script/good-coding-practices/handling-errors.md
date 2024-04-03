---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:06.000072-07:00
description: "Google Apps Script\uC5D0\uC11C\uC758 \uC624\uB958 \uCC98\uB9AC\uB294\
  \ \uC2A4\uD06C\uB9BD\uD2B8 \uC2E4\uD589 \uC911\uC5D0 \uBC1C\uC0DD\uD560 \uC218 \uC788\
  \uB294 \uC608\uC678\uB098 \uC624\uB958\uB97C \uC608\uCE21\uD558\uACE0, \uCE90\uCE58\
  \uD558\uBA70, \uB300\uC751\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC2A4\uD06C\uB9BD\uD2B8\uAC00\
  \ \uC608\uC0C1\uCE58 \uBABB\uD55C \uC2E4\uD328\uB85C\uBD80\uD130 \uBCF4\uD638\uB418\
  \uB3C4\uB85D \uD558\uC5EC, \uB354\uC6B1 \uBD80\uB4DC\uB7FD\uACE0 \uC0AC\uC6A9\uC790\
  \ \uCE5C\uD654\uC801\uC778 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC744 \uBCF4\uC7A5\
  \uD558\uACE0,\u2026"
lastmod: '2024-03-13T22:44:54.543143-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C\uC758 \uC624\uB958 \uCC98\uB9AC\uB294 \uC2A4\
  \uD06C\uB9BD\uD2B8 \uC2E4\uD589 \uC911\uC5D0 \uBC1C\uC0DD\uD560 \uC218 \uC788\uB294\
  \ \uC608\uC678\uB098 \uC624\uB958\uB97C \uC608\uCE21\uD558\uACE0, \uCE90\uCE58\uD558\
  \uBA70, \uB300\uC751\uD558\uB294 \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4\
  ."
title: "\uC624\uB958 \uCC98\uB9AC\uD558\uAE30"
weight: 16
---

## 무엇이며 왜인가?

Google Apps Script에서의 오류 처리는 스크립트 실행 중에 발생할 수 있는 예외나 오류를 예측하고, 캐치하며, 대응하는 것에 관한 것입니다. 프로그래머들은 스크립트가 예상치 못한 실패로부터 보호되도록 하여, 더욱 부드럽고 사용자 친화적인 애플리케이션을 보장하고, 갑작스러운 충돌 없이 오류를 관리하거나 기록할 수 있도록 이를 구현합니다.

## 어떻게:

Google Apps Script는 JavaScript에 기반을 둔 것이므로, 성공 여부와 관계없이 정리가 필요한 경우 `finally`와 함께 전통적인 `try-catch`문을 오류 처리에 사용할 수 있습니다.

```javascript
function myFunction() {
  try {
    // 오류를 발생시킬 수 있는 코드
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("A1 셀이 비어 있습니다.");
    }
    Logger.log(data);
  } catch (e) {
    // 오류 처리 코드
    Logger.log("오류: " + e.message);
  } finally {
    // 오류 발생 여부와 상관없이 실행되는 정리 코드
    Logger.log("함수가 완료되었습니다.");
  }
}
```

오류가 없는 경우의 샘플 출력:
```
[셀 값]
함수가 완료되었습니다.
```

오류가 있는 경우의 샘플 출력(예를 들어 A1이 비어 있는 경우):
```
오류: A1 셀이 비어 있습니다.
함수가 완료되었습니다.
```

Google Apps Script는 또한 필요에 따라 특정 오류 유형을 캐치하고 `Error` 객체를 사용하여 사용자 정의 오류를 발생시키는 것을 지원합니다. 그러나 고급 오류 분류의 부재는 오류 메시지에 의존하는 것이 필수적으로 만듭니다.

## 심층 탐구

역사적으로, JavaScript(및 확장으로 Google Apps Script)와 같은 스크립팅 언어에서의 오류 처리는 상세한 예외 계층 및 포괄적인 디버깅 도구와 같은 기능을 제공하는 일부 컴파일 언어보다는 덜 정교했습니다. Google Apps Script의 모델은 비교적 간단하며 JavaScript의 `try-catch-finally` 패러다임을 활용합니다. 이 간단함은 Google의 생태계 내에서 소규모에서 중규모 규모의 애플리케이션을 신속하게 개발하고 배포하기 위한 언어의 설계와 일치하지만, 복잡한 오류 시나리오를 다루는 개발자들에게는 때때로 제한이 될 수 있습니다.

보다 복잡한 애플리케이션에서, 프로그래머들은 종종 Google Apps Script의 기본 오류 처리를 사용자 정의 로깅 및 오류 보고 메커니즘으로 보완합니다. 이는 감사를 위해 Google 시트에 오류를 기록하거나 Google Apps Script의 URL Fetch 서비스를 통해 스크립트 환경 밖으로 오류 세부 사항을 보내는 제3자 로깅 서비스를 사용하는 것을 포함할 수 있습니다.

Google Apps Script가 Java나 C# 같은 언어에 비해 내장 오류 처리의 복잡성 및 기능 측면에서 뒤처질 수 있지만, Google 서비스와의 통합 및 `try-catch-finally` 접근법의 간단함은 Google 생태계 내에서 작업을 빠르게 자동화하고 통합을 만들기 위한 개발자들에게 강력한 도구를 제공합니다. 다른 배경을 가진 개발자들은 복잡한 오류 처리 패턴을 마스터하는 것이 아니라, 사용 가능한 것을 창의적으로 활용하여 스크립트를 견고하고 사용자 친화적으로 만드는 데에 도전을 발견할 수 있습니다.
