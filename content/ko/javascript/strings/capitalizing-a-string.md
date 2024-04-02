---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:56.250826-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uBA74\uC11C \uB098\uBA38\uC9C0 \uBB38\uC790\uB4E4\uC740\
  \ \uADF8\uB300\uB85C \uC720\uC9C0\uD558\uB294 \uC791\uC5C5\uC744 \uBB38\uC790\uC5F4\
  \ \uB300\uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740\
  \ \uC0AC\uC6A9\uC790 \uC785\uB825\uC744 \uD3EC\uB9F7\uD305\uD558\uAC70\uB098, \uC774\
  \uB984\uC774\uB098 \uC81C\uBAA9\uC744 \uD45C\uC2DC\uD558\uACE0 \uC0AC\uC6A9\uC790\
  \ \uC778\uD130\uD398\uC774\uC2A4 \uD14D\uC2A4\uD2B8\uC758 \uC77C\uAD00\uC131\uC744\
  \ \uD655\uBCF4\uD558\uAE30 \uC704\uD574 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\
  \uC11C \uD754\uD788 \uC218\uD589\uB429\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.767866-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uBA74\uC11C \uB098\uBA38\uC9C0 \uBB38\uC790\uB4E4\uC740 \uADF8\
  \uB300\uB85C \uC720\uC9C0\uD558\uB294 \uC791\uC5C5\uC744 \uBB38\uC790\uC5F4 \uB300\
  \uBB38\uC790\uD654\uB77C\uACE0 \uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uC0AC\
  \uC6A9\uC790 \uC785\uB825\uC744 \uD3EC\uB9F7\uD305\uD558\uAC70\uB098, \uC774\uB984\
  \uC774\uB098 \uC81C\uBAA9\uC744 \uD45C\uC2DC\uD558\uACE0 \uC0AC\uC6A9\uC790 \uC778\
  \uD130\uD398\uC774\uC2A4 \uD14D\uC2A4\uD2B8\uC758 \uC77C\uAD00\uC131\uC744 \uD655\
  \uBCF4\uD558\uAE30 \uC704\uD574 \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uC5D0\uC11C\
  \ \uD754\uD788 \uC218\uD589\uB429\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 무엇인가 & 왜인가?
문자열의 첫 글자를 대문자로 변환하면서 나머지 문자들은 그대로 유지하는 작업을 문자열 대문자화라고 합니다. 이 작업은 사용자 입력을 포맷팅하거나, 이름이나 제목을 표시하고 사용자 인터페이스 텍스트의 일관성을 확보하기 위해 자바스크립트에서 흔히 수행됩니다.

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
