---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:10.457755-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Google Apps Script\uC5D0\uC11C \uC815\uADDC\
  \ \uD45C\uD604\uC2DD\uC744 \uC0AC\uC6A9\uD558\uB294 \uAC83\uC740 JavaScript \uAE30\
  \uBC18 \uAD6C\uBB38 \uB355\uBD84\uC5D0 \uAC04\uB2E8\uD569\uB2C8\uB2E4. \uB2E4\uC74C\
  \uC740 \uAC80\uC0C9\uACFC \uB370\uC774\uD130 \uAC80\uC99D\uACFC \uAC19\uC740 \uC77C\
  \uBC18\uC801\uC778 \uC791\uC5C5\uC5D0 \uC815\uADDC\uC2DD\uC744 \uC2A4\uD06C\uB9BD\
  \uD2B8\uC5D0 \uD1B5\uD569\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-04-05T21:53:56.387643-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC0AC\
  \uC6A9\uD558\uB294 \uAC83\uC740 JavaScript \uAE30\uBC18 \uAD6C\uBB38 \uB355\uBD84\
  \uC5D0 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95"
weight: 11
---

## 사용 방법:
Google Apps Script에서 정규 표현식을 사용하는 것은 JavaScript 기반 구문 덕분에 간단합니다. 다음은 검색과 데이터 검증과 같은 일반적인 작업에 정규식을 스크립트에 통합하는 방법입니다.

### 문자열 검색
특정 패턴, 예를 들어 이메일 주소가 문자열에 포함되어 있는지 찾고자 한다고 가정해 봅시다. 다음은 간단한 예입니다:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("찾음: " + found[0]);
  } else {
    Logger.log("이메일을 찾지 못함.");
  }
}

// 사용 예
findEmailInText("info@example.com으로 연락주세요.");
```

### 데이터 검증
정규 표현식은 데이터 검증에서 빛을 발합니다. 아래는 입력 문자열이 간단한 비밀번호 정책(최소 한 개의 대문자, 한 개의 소문자, 그리고 최소 8자)을 준수하는지 검증하는 함수입니다.

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// 예제 출력
Logger.log(validatePassword("Str0ngPass")); // 출력: true
Logger.log(validatePassword("weak"));       // 출력: false
```

## 심층 탐구
Google Apps Script에서 정규 표현식은 1997년 6월 ECMAScript 언어 사양에서 처음 표준화된 JavaScript에서 상속됩니다. 비록 강력하지만, 때때로 이해하기 어렵고 유지 관리하기 어려운 코드로 이어질 수 있으며, 다른 파싱 방법을 통해 보다 효율적으로 해결될 수 있는 복잡한 패턴 매칭 작업에 사용될 때 특히 그렇습니다.

예를 들어, HTML이나 XML 파싱에 정규식을 사용할 수는 있지만, 이 문서들의 중첩되고 복잡한 구조로 인해 일반적으로 권장되지 않습니다. 대신, HTML을 위한 DOM 파서와 같이 이러한 구조를 파싱하기 위해 특별히 설계된 도구가 더 신뢰성 있고 읽기 쉽습니다.

또한, Google Apps Script 개발자들은 대규모 텍스트 조작 작업에서 복잡한 정규 표현식 패턴을 사용할 때 잠재적 성능 문제에 유념해야 합니다. 정규 표현식 처리는 CPU를 많이 사용할 수 있습니다. 이러한 경우, 작업을 더 간단한 하위 작업으로 나누거나 내장된 문자열 조작 함수를 사용하는 것이 성능과 유지 관리의 더 좋은 균형을 제공할 수 있습니다.
