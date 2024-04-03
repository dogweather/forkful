---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:58.511053-07:00
description: "\uBC29\uBC95: TypeScript\uB294 JavaScript\uC758 \uC0C1\uC704 \uC9D1\uD569\
  \uC73C\uB85C, \uC21C\uC218 JavaScript \uBC29\uC2DD\uBD80\uD130 \uB354 \uBCF5\uC7A1\
  \uD558\uAC70\uB098 \uD2B9\uC815 \uC0AC\uC6A9 \uC0AC\uB840\uC5D0 \uB354 \uC801\uD569\
  \uD55C \uC11C\uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\uC6A9\
  \uD558\uB294 \uB2E4\uC591\uD55C \uBC29\uBC95\uC73C\uB85C \uBB38\uC790\uC5F4\uC744\
  \ \uB300\uBB38\uC790\uD654\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. **\uC21C\uC218\
  \ JavaScript \uC811\uADFC\uBC95:**."
lastmod: '2024-03-13T22:44:54.830087-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\uB294 JavaScript\uC758 \uC0C1\uC704 \uC9D1\uD569\uC73C\uB85C\
  , \uC21C\uC218 JavaScript \uBC29\uC2DD\uBD80\uD130 \uB354 \uBCF5\uC7A1\uD558\uAC70\
  \uB098 \uD2B9\uC815 \uC0AC\uC6A9 \uC0AC\uB840\uC5D0 \uB354 \uC801\uD569\uD55C \uC11C\
  \uB4DC\uD30C\uD2F0 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uD65C\uC6A9\uD558\uB294\
  \ \uB2E4\uC591\uD55C \uBC29\uBC95\uC73C\uB85C \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\
  \uC790\uD654\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 방법:
TypeScript는 JavaScript의 상위 집합으로, 순수 JavaScript 방식부터 더 복잡하거나 특정 사용 사례에 더 적합한 서드파티 라이브러리를 활용하는 다양한 방법으로 문자열을 대문자화할 수 있습니다.

**순수 JavaScript 접근법:**

```typescript
function capitalize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

// 샘플 출력:
console.log(capitalize('hello TypeScript!')); // 'Hello TypeScript!'
```

이 방법은 간단하며 `charAt()` 메소드를 사용하여 문자열의 첫 번째 문자에 접근하고 `toUpperCase()`로 대문자로 변환합니다. 그 다음 `slice(1)` 메소드로 문자열의 나머지 부분을 그대로 가져옵니다.

**Lodash 라이브러리 사용하기:**

이미 [Lodash](https://lodash.com/) 라이브러리를 사용 중인 프로젝트의 경우 더 적은 보일러플레이트 코드로 같은 결과를 달성할 수 있는 `_.capitalize` 함수를 활용할 수 있습니다.

먼저, Lodash를 설치하세요:

```bash
npm install lodash
```

그 다음, TypeScript 파일에서 사용하세요:

```typescript
import * as _ from 'lodash';

// 샘플 출력:
console.log(_.capitalize('hello TypeScript!')); // 'Hello typescript!'
```

참고: Lodash의 `_.capitalize` 메소드는 문자열의 나머지 부분을 소문자로 변환하므로 항상 원하는 결과가 아닐 수 있습니다.

**정규 표현식 사용하기:**

정규 표현식을 사용하면 문자열의 첫 글자를 대문자화하는 간결한 방법을 제공할 수 있으며, 특히 문자열의 각 단어의 첫 글자를 대문자화해야 하는 경우 유용합니다.

```typescript
function capitalizeWords(str: string): string {
  return str.replace(/\b\w/g, char => char.toUpperCase());
}

// 샘플 출력:
console.log(capitalizeWords('hello typescript world!')); // 'Hello Typescript World!'
```

이 방법은 `replace()` 함수를 사용하여 단어 경계 뒤에 오는 영숫자 문자(`\b\w`)를 찾아 각각을 대문자화합니다. 특히 제목이나 헤딩에 유용합니다.
