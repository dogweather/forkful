---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:58.511053-07:00
description: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294 \uAC83\
  \uC740 \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uCCAB \uBC88\uC9F8 \uBB38\uC790\
  \uB97C \uC18C\uBB38\uC790\uC778 \uACBD\uC6B0 \uB300\uBB38\uC790\uB85C \uBCC0\uACBD\
  \uD558\uB294 \uC791\uC5C5\uC744 \uB9D0\uD558\uBA70, \uB300\uBD80\uBD84 \uBB38\uC790\
  \uC5F4\uC758 \uB098\uBA38\uC9C0 \uBD80\uBD84\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\
  \uC2B5\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB300\uBA85\uC0AC\uB098 \uBB38\uC7A5\
  \uC758 \uC2DC\uC791\uC774 \uBB38\uBC95 \uADDC\uCE59\uC744 \uC900\uC218\uD558\uB3C4\
  \uB85D \uD558\uC5EC \uD14D\uC2A4\uD2B8 \uCC98\uB9AC \uC2DC \uACB0\uACFC\uBB3C\uC774\
  \ \uC804\uBB38\uC801\uC774\uACE0 \uC77D\uAE30 \uC27D\uAC8C \uB9CC\uB4DC\uB294 \uB370\
  \u2026"
lastmod: '2024-03-13T22:44:54.830087-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294 \uAC83\uC740\
  \ \uC8FC\uC5B4\uC9C4 \uBB38\uC790\uC5F4\uC758 \uCCAB \uBC88\uC9F8 \uBB38\uC790\uB97C\
  \ \uC18C\uBB38\uC790\uC778 \uACBD\uC6B0 \uB300\uBB38\uC790\uB85C \uBCC0\uACBD\uD558\
  \uB294 \uC791\uC5C5\uC744 \uB9D0\uD558\uBA70, \uB300\uBD80\uBD84 \uBB38\uC790\uC5F4\
  \uC758 \uB098\uBA38\uC9C0 \uBD80\uBD84\uC740 \uBCC0\uACBD\uD558\uC9C0 \uC54A\uC2B5\
  \uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 무엇인가 & 왜인가?
문자열을 대문자화하는 것은 주어진 문자열의 첫 번째 문자를 소문자인 경우 대문자로 변경하는 작업을 말하며, 대부분 문자열의 나머지 부분은 변경하지 않습니다. 이 작업은 대명사나 문장의 시작이 문법 규칙을 준수하도록 하여 텍스트 처리 시 결과물이 전문적이고 읽기 쉽게 만드는 데 주로 사용됩니다.

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
