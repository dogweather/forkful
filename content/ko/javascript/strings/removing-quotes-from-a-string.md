---
date: 2024-01-26 03:40:55.374115-07:00
description: "\uBC29\uBC95: \uB354\uBE14 \uB530\uC634\uD45C\uB85C \uAC10\uC2F8\uC9C4\
  \ \uBB38\uC790\uC5F4, \uC608\uB97C \uB4E4\uC5B4 `\"\\\"Hello, World!\\\"\"` \uAC00\
  \ \uC788\uACE0, \uB530\uC634\uD45C \uC5C6\uB294 \uC21C\uC218\uD55C \uD14D\uC2A4\uD2B8\
  \uB97C \uC6D0\uD55C\uB2E4\uACE0 \uAC00\uC815\uD574 \uBD05\uC2DC\uB2E4. \uC5EC\uAE30\
  \ \uB530\uC634\uD45C\uC758 \uAD74\uB808\uC5D0\uC11C \uBB38\uC790\uC5F4\uC744 \uD574\
  \uBC29\uC2DC\uD0AC \uBE60\uB978 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8 \uC2A4\uB2C8\
  \uD3AB\uC774 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.774271-06:00'
model: gpt-4-0125-preview
summary: "\uB354\uBE14 \uB530\uC634\uD45C\uB85C \uAC10\uC2F8\uC9C4 \uBB38\uC790\uC5F4\
  , \uC608\uB97C \uB4E4\uC5B4 `\"\\\"Hello, World!\\\"\"` \uAC00 \uC788\uACE0, \uB530\
  \uC634\uD45C \uC5C6\uB294 \uC21C\uC218\uD55C \uD14D\uC2A4\uD2B8\uB97C \uC6D0\uD55C\
  \uB2E4\uACE0 \uAC00\uC815\uD574 \uBD05\uC2DC\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 방법:
더블 따옴표로 감싸진 문자열, 예를 들어 `"\"Hello, World!\""` 가 있고, 따옴표 없는 순수한 텍스트를 원한다고 가정해 봅시다. 여기 따옴표의 굴레에서 문자열을 해방시킬 빠른 자바스크립트 스니펫이 있습니다:

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // 출력: Hello, World!
```

그리고 만약 단일 따옴표를 다루고 있다면? 정규식을 조금 조정하기만 하면 됩니다:

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // 출력: Hello, World!
```

또는 문자열이 둘 다를 혼합하고 있다면? 문제 없습니다:

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // 출력: 'Hello, World!'
```

## 심층 분석
JSON이 주류가 되기 전에, 따옴표를 이스케이프하는 것은 백슬래시와 해킹의 야생이었습니다. 초기 프로그래밍 언어는 항상 따옴표와 잘 작동하지 않았기 때문에 많은 수동 문자열 조작이 필요했습니다. 이제 표준화된 데이터 형식으로, 따옴표를 제거하는 것은 종종 입력을 정리하는 것에 관한 것이며, JSON으로 처리되기 전이나 텍스트가 서식 충돌 없이 저장됩니다.

`.replace()`의 대안이 있나요? 물론입니다! 따옴표를 기준으로 문자열을 나누고 합치기, 따옴표의 위치를 확실히 안다면 slice를 사용하기, 또는 필요한 텍스트를 뽑아내기 위해 정규식 매치를 사용하기 등이 있습니다. 모두 상황에 따라 달라집니다.

하지만 따옴표 안의 따옴표, 이스케이프된 따옴표, 국제 문자 등의 엣지 케이스를 잊지 마세요. 문자열을 예외의 잠재적인 지뢰밭으로 생각하고, 신중히 다루세요. 현대 자바스크립트 엔진은 정규식 연산을 효율적으로 처리하도록 최적화되어 있으므로 일반적으로 가장 좋은 선택이지만, 무거운 데이터 처리 작업의 성능을 항상 확인하는 것이 가치가 있습니다.

## 참고 자료
문자열 조작과 정규식에 대해 더 깊이 파고들어 보세요:

- 문자열.replace()에 대한 Mozilla Developer Network: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- 정규식 패턴을 테스트할 수 있는 Regex101: https://regex101.com/
- 현대 웹 개발에서 우리가 많은 따옴표를 다루는 이유를 이해하기 위한 JSON.org: http://json.org/
