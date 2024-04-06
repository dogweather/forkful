---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:56.250826-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\
  \uC5D0\uB294 \uBB38\uC790\uC5F4\uC744 \uC9C1\uC811 \uB300\uBB38\uC790\uD654\uD558\
  \uB294 \uB0B4\uC7A5 \uBA54\uC18C\uB4DC\uAC00 \uC5C6\uC9C0\uB9CC \uAE30\uBCF8 \uBB38\
  \uC790\uC5F4 \uC870\uC791 \uBC29\uBC95\uC744 \uC0AC\uC6A9\uD574 \uC27D\uAC8C \uAD6C\
  \uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.767866-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uB294 \uBB38\uC790\uC5F4\uC744\
  \ \uC9C1\uC811 \uB300\uBB38\uC790\uD654\uD558\uB294 \uB0B4\uC7A5 \uBA54\uC18C\uB4DC\
  \uAC00 \uC5C6\uC9C0\uB9CC \uAE30\uBCF8 \uBB38\uC790\uC5F4 \uC870\uC791 \uBC29\uBC95\
  \uC744 \uC0AC\uC6A9\uD574 \uC27D\uAC8C \uAD6C\uD604\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 어떻게 하나:
자바스크립트에는 문자열을 직접 대문자화하는 내장 메소드가 없지만 기본 문자열 조작 방법을 사용해 쉽게 구현할 수 있습니다.

### 표준 자바스크립트 사용하기
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // 출력: "Hello world"
```

### ES6 버전
ES6 템플릿 리터럴을 사용하면, 함수를 더 간결하게 작성할 수 있습니다:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // 출력: "Hello ES6"
```

### Lodash 사용하기
Lodash는 문자열을 포함하여 자바스크립트 값들을 조작하고 작업하기 위한 다양한 함수를 제공하는 인기 있는 타사 유틸리티 라이브러리입니다. Lodash를 사용하여 문자열을 대문자화하려면:
```javascript
// 먼저 lodash를 설치하지 않았다면 설치하세요: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // 출력: "Lodash example"
```
_Lodash는 첫 글자를 대문자화하는 것뿐만 아니라 문자열의 나머지 부분을 소문자로 변환한다는 점에서 평범한 자바스크립트 구현과 약간 다릅니다._

### CSS 사용하기 (단지 표시 목적일 때)
UI에서 텍스트를 대문자로 표시하려는 목적이라면 CSS를 사용할 수 있습니다:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- "Hello css"로 표시됩니다 -->
```
**참고:** 이 방법은 웹페이지에 텍스트가 어떻게 보이는지를 변경할 뿐, 자바스크립트 자체에서 문자열을 변경하지는 않습니다.
