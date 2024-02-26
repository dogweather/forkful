---
date: 2024-01-26 03:43:25.792114-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\
  \uD55C\uB2E4\uB294 \uAC83\uC740 \uCF54\uB4DC\uC5D0\uC11C \uBB38\uC790\uC5F4 \uB9AC\
  \uD130\uB7F4\uC744 \uC815\uC758\uD558\uB294 \uB458\uB7EC\uC2FC \uB2E8\uC77C (`'`)\
  \ \uB610\uB294 \uC774\uC911 (`\"`) \uB530\uC634\uD45C \uBB38\uC790\uB97C \uC81C\uAC70\
  \uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC774 \uC774 \uC791\uC5C5\uC744 \uD558\uB294 \uC5EC\uB7EC \uAC00\uC9C0\
  \ \uC774\uC720\uB85C\uB294 \uCD9C\uB825 \uD615\uC2DD \uC9C0\uC815, \uC0AC\uC6A9\uC790\
  \ \uC785\uB825\uC758 \uC0B4\uADE0, \uB530\uC634\uD45C\uAC00 \uBD88\uD544\uC694\uD558\
  \uAC70\uB098 \uC624\uB958\uB97C\u2026"
lastmod: '2024-02-25T18:49:51.803449-07:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uCF54\uB4DC\uC5D0\uC11C \uBB38\uC790\uC5F4 \uB9AC\uD130\
  \uB7F4\uC744 \uC815\uC758\uD558\uB294 \uB458\uB7EC\uC2FC \uB2E8\uC77C (`'`) \uB610\
  \uB294 \uC774\uC911 (`\"`) \uB530\uC634\uD45C \uBB38\uC790\uB97C \uC81C\uAC70\uD558\
  \uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC774 \uC774 \uC791\uC5C5\uC744 \uD558\uB294 \uC5EC\uB7EC \uAC00\uC9C0 \uC774\
  \uC720\uB85C\uB294 \uCD9C\uB825 \uD615\uC2DD \uC9C0\uC815, \uC0AC\uC6A9\uC790 \uC785\
  \uB825\uC758 \uC0B4\uADE0, \uB530\uC634\uD45C\uAC00 \uBD88\uD544\uC694\uD558\uAC70\
  \uB098 \uC624\uB958\uB97C\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열에서 따옴표를 제거한다는 것은 코드에서 문자열 리터럴을 정의하는 둘러싼 단일 (`'`) 또는 이중 (`"`) 따옴표 문자를 제거하는 것을 의미합니다. 프로그래머들이 이 작업을 하는 여러 가지 이유로는 출력 형식 지정, 사용자 입력의 살균, 따옴표가 불필요하거나 오류를 일으킬 수 있는 파싱 또는 저장을 위한 문자열 준비 등이 있습니다.

## 방법:
TypeScript에서 문자열에서 그 성가신 따옴표들을 제거하는 무심한 가이드가 여기 있습니다.

```typescript
// 옵션 A: 정규표현식을 사용하여 단일 또는 이중 따옴표 교체
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"따옴표가 있는 문자열"`)); // 따옴표가 있는 문자열
console.log(removeQuotes(`'또 다른 하나'`)); // 또 다른 하나

// 옵션 B: 시작과 끝이 다른 따옴표를 가진 문자열 다루기
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"불일치하는 따옴표'`)); // "불일치하는 따옴표'

// 옵션 C: 여러 종류의 따옴표 제거
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'믹스앤매치'"`)); // 믹스앤매치
```

## 심화 학습
TypeScript가 아직 개념조차 없던 옛날부터, JavaScript 코더들은 이미 따옴표 짓궂은 장난에 대처하고 있었고, TypeScript에 대한 이야기도 대체로 같습니다. 시대가 변함에 따라, 우리가 문자열을 자르는 방식도 변합니다. 요즘에는 정규표현식의 근육질 힘으로, 우리는 서투른 문자열 슬라이싱이나 다른 지루한 방법을 사용하는 것을 제쳐둡니다.

위의 예제들이 대부분의 필요를 충족시켜야 하지만, 따옴표는 복잡해질 수 있습니다. 중첩된, 불일치하는, 이스케이프된 따옴표들이 여러분을 넘어뜨리려고 기다리는 장난꾼들입니다. 이런 경우에는 더 정교한 패턴이나 심지어 파서도 필요할 수 있어 모든 복잡한 사례를 처리할 수 있습니다.

대안들? 어떤 사람들은 `trim`과 `trimStart` / `trimEnd` 같은 메소드를 가진 lodash와 같은 라이브러리를 선호합니다. 원하는 문자를 잘라내도록 설정하면 따옴표를 자르는 데에 맞춤 설정할 수 있습니다.

그리고 여러분 TypeScript 열정가들을 위해, 타입에 대해서 잊지 말아야합니다. 여기서는 주로 문자열을 다루지만, 사용자 입력이나 파싱을 다룰 때 타입 가드나 심지어 제네릭을 이용하면 코드를 안전하게 유지하는 데 도움이 됩니다.

## 참고 자료
더 많은 정보를 위한 가상의 장소들을 확인하세요:

- MDN 웹 문서에서 정규표현식에 대해 (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript 공식 문서 (https://www.typescriptlang.org/docs/)
- 라이브러리가 필요 없는 경우에 대한 lodash/Underscore – 문자열 도우미 (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: 수많은 개발자들이 따옴표 재난을 겪어본 참호를 탐험하세요 (https://stackoverflow.com/)
