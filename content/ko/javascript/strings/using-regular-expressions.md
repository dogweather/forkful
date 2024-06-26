---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:23.505515-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: \uC2DC\uC791\uD558\uB824\uBA74, \uAC04\uB2E8\
  \uD55C regex \uD328\uD134\uC744 \uC0DD\uC131\uD558\uACE0 \uBB38\uC790\uC5F4\uC5D0\
  \uC11C \uC77C\uCE58 \uD56D\uBAA9\uC744 \uCC3E\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4\
  . \uC5EC\uAE30\uC11C, \uC6B0\uB9AC\uB294 \"code\"\uB77C\uB294 \uB2E8\uC5B4\uB97C\
  \ \uCC3E\uC744 \uAC83\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.777060-06:00'
model: gpt-4-0125-preview
summary: "\uC2DC\uC791\uD558\uB824\uBA74, \uAC04\uB2E8\uD55C regex \uD328\uD134\uC744\
  \ \uC0DD\uC131\uD558\uACE0 \uBB38\uC790\uC5F4\uC5D0\uC11C \uC77C\uCE58 \uD56D\uBAA9\
  \uC744 \uCC3E\uC744 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 사용 방법:


### 기본 일치
시작하려면, 간단한 regex 패턴을 생성하고 문자열에서 일치 항목을 찾을 수 있습니다. 여기서, 우리는 "code"라는 단어를 찾을 것입니다:

```javascript
const str = "I love to code in JavaScript.";
const pattern = /code/;
const result = pattern.test(str);
console.log(result); // true
```

### `String.prototype.match()` 사용
일치하는 항목의 배열을 검색하려면:

```javascript
const matches = str.match(/code/);
console.log(matches[0]); // "code"
console.log(matches.index); // 10
```

### 전역 검색
모든 일치 항목을 찾으려면 `g` 플래그를 사용합니다:

```javascript
const globalMatches = str.match(/o/g);
console.log(globalMatches); // ["o", "o", "o"]
```

### 대소문자 구분 없는 일치
`i` 플래그는 대소문자를 구분하지 않습니다:

```javascript
const caseInsensitiveMatch = "JavaScript is fun".match(/javascript/i);
console.log(caseInsensitiveMatch[0]); // "JavaScript"
```

### 텍스트 바꾸기
`String.prototype.replace()`를 사용하여 문자열의 일부를 바꿉니다:

```javascript
const newStr = "JavaScript is fun".replace(/fun/, "awesome");
console.log(newStr); // "JavaScript is awesome"
```

### 그룹 사용
그룹은 패턴의 일부를 포착할 수 있습니다:

```javascript
const groupedPattern = /(\w+) is (\w+)/;
const replaceWithGroups = "JavaScript is fun".replace(groupedPattern, "$2 is $1");
console.log(replaceWithGroups); // "fun is JavaScript"
```

### 타사 라이브러리
자바스크립트의 내장 정규표현식 기능이 강력하지만, `XRegExp`와 같은 라이브러리를 사용하면 일부 작업이 간소화될 수 있습니다. 이는 추가 문법과 플래그를 제공하여 복잡한 패턴을 더 읽기 쉽게 만듭니다:

```javascript
// XRegExp 라이브러리 예제
const XRegExp = require('xregexp');
const str = "Cats are fantastic.";
const unicodeWordMatch = XRegExp.match(str, XRegExp('\\p{L}+'), 'all');
console.log(unicodeWordMatch); // ["Cats", "are", "fantastic"]
```

이 코드 스니펫은 문자열에서 모든 유니코드 단어를 일치시키기 위해 `XRegExp`를 사용하는 방법을 보여주며, 라이브러리가 자바스크립트의 내장 기능을 넘어 확장된 문자 집합을 처리할 수 있는 능력을 보여줍니다.
