---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:58.211828-07:00
description: "\uBC29\uBC95: Google Apps Script\uC5D0\uC11C\uB294, \uC790\uBC14\uC2A4\
  \uD06C\uB9BD\uD2B8\uB97C \uAE30\uBC18\uC73C\uB85C \uD558\uACE0 \uC788\uC73C\uBA70\
  , \uBB38\uC790\uC5F4\uC744 \uACB0\uD569\uD558\uB294 \uBA87 \uAC00\uC9C0 \uBC29\uBC95\
  \uC774 \uC788\uC2B5\uB2C8\uB2E4. \uC5EC\uAE30 \uBA87 \uAC00\uC9C0 \uC77C\uBC18\uC801\
  \uC778 \uBC29\uBC95\uB4E4\uC774 \uC788\uC2B5\uB2C8\uB2E4: #."
lastmod: '2024-03-13T22:44:54.515282-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\uC5D0\uC11C\uB294, \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \uB97C \uAE30\uBC18\uC73C\uB85C \uD558\uACE0 \uC788\uC73C\uBA70, \uBB38\uC790\uC5F4\
  \uC744 \uACB0\uD569\uD558\uB294 \uBA87 \uAC00\uC9C0 \uBC29\uBC95\uC774 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## 방법:
Google Apps Script에서는, 자바스크립트를 기반으로 하고 있으며, 문자열을 결합하는 몇 가지 방법이 있습니다. 여기 몇 가지 일반적인 방법들이 있습니다:

### 더하기 연산자(`+`) 사용하기:
```javascript
var firstName = "John";
var lastName = "Doe";
var fullName = firstName + " " + lastName;
Logger.log(fullName); // 출력: John Doe
```

### `concat()` 메서드 사용하기:
```javascript
var string1 = "Hello";
var string2 = "World";
var combinedString = string1.concat(" ", string2);
Logger.log(combinedString); // 출력: Hello World
```

### 템플릿 리터럴(백틱) 사용하기:
이는 문자열 내에 표현식을 쉽게 포함할 수 있게 해주는, 현대적이고 유연한 문자열 결합 방법입니다.

```javascript
var language = "Google Apps Script";
var message = `Learning ${language} is fun!`;
Logger.log(message); // 출력: Learning Google Apps Script is fun!
```

이들 각각의 방법은 사용 사례가 있으며, 선택은 일반적으로 가독성 요구 사항과 결합되는 문자열의 복잡성에 따라 달라집니다.

## 심화 학습
문자열 결합은 Google Apps Script뿐만 아니라 많은 프로그래밍 언어의 기본적인 측면입니다. 역사적으로, 문자열 결합은 종종 더하기 연산자나 `concat()`과 같은 특별한 함수/메소드를 사용하여 수행되었습니다. 하지만, ECMAScript 2015 (ES6)에서 템플릿 리터럴의 도입으로, Google Apps Script가 지원하는 바, 개발자들은 문자열을 다루는 데 있어 보다 강력하고 직관적인 방법을 얻게 되었습니다.

템플릿 리터럴은 문자열 내에 표현식을 포함시키기 위한 문법을 단순화할 뿐만 아니라 명시적인 개행 문자 없이도 여러 줄의 문자열을 지원합니다. 이는 가능한 오류를 줄이고, 복잡한 문자열을 다루거나 텍스트 템플릿에 여러 변수를 대체할 때 코드 가독성을 향상시킵니다.

`+` 연산자와 `concat()` 메소드는 여전히 후방 호환성 및 단순한 시나리오에서의 단순함을 위해 널리 사용되고 지원되지만, 템플릿 리터럴은 특히 가독성과 유지보수성이 중요한 경우 문자열 결합을 위한 현대적이고 표현력 있는 대안으로 종종 우월하다고 여겨집니다.

그럼에도 불구하고, 프로젝트의 특정 맥락과 요구 사항에 가장 잘 맞는 방법을 선택하는 것이 중요합니다. 이때 Google Apps Script와의 호환성 문제(드물게 발생하는 문제이지만), 성능 영향(대부분의 애플리케이션에는 미미함), 그리고 개발 팀이 현대 자바스크립트 기능에 익숙한 정도 등과 같은 요소를 고려해야 합니다.
