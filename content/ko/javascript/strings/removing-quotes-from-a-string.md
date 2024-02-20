---
date: 2024-01-26 03:40:55.374115-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C \uAF2C\uC77C \uC218 \uC788\uB294\
  \ \uADF8 \uADC0\uCC2E\uC740 \uB530\uC634\uD45C\uB97C \uC5C6\uC560\uB294 \uAC83\uC744\
  \ \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD2B9\uD788 \uB370\uC774\uD130\uB97C \uD30C\uC2F1\
  \uD558\uAC70\uB098 JSON \uAC1D\uCCB4\uB97C \uAD6C\uC131\uD560 \uB54C \uC911\uC694\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\uAC12\
  \uC744 \uC815\uD654\uD558\uACE0, \uAD6C\uBB38 \uC624\uB958\uB97C \uD53C\uD558\uBA70\
  , \uBB38\uC790\uC5F4\uC774 \uCF54\uB4DC\uC758 \uB2E4\uB978 \uBD80\uBD84\uACFC \uC798\
  \ \uB3D9\uC791\uD558\uB3C4\uB85D \uD558\uAE30 \uC704\uD574\u2026"
lastmod: 2024-02-19 22:05:14.685850
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uCF54\uB4DC\uB97C \uAF2C\uC77C \uC218 \uC788\uB294 \uADF8\
  \ \uADC0\uCC2E\uC740 \uB530\uC634\uD45C\uB97C \uC5C6\uC560\uB294 \uAC83\uC744 \uC758\
  \uBBF8\uD569\uB2C8\uB2E4. \uD2B9\uD788 \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD558\
  \uAC70\uB098 JSON \uAC1D\uCCB4\uB97C \uAD6C\uC131\uD560 \uB54C \uC911\uC694\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC785\uB825\uAC12\uC744\
  \ \uC815\uD654\uD558\uACE0, \uAD6C\uBB38 \uC624\uB958\uB97C \uD53C\uD558\uBA70,\
  \ \uBB38\uC790\uC5F4\uC774 \uCF54\uB4DC\uC758 \uB2E4\uB978 \uBD80\uBD84\uACFC \uC798\
  \ \uB3D9\uC791\uD558\uB3C4\uB85D \uD558\uAE30 \uC704\uD574\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇을, 왜?
문자열에서 따옴표를 제거한다는 것은 코드를 꼬일 수 있는 그 귀찮은 따옴표를 없애는 것을 의미합니다. 특히 데이터를 파싱하거나 JSON 객체를 구성할 때 중요합니다. 프로그래머들은 입력값을 정화하고, 구문 오류를 피하며, 문자열이 코드의 다른 부분과 잘 동작하도록 하기 위해 이 작업을 합니다.

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
