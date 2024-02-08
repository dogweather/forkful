---
title:                "문자열 대문자화"
aliases:
- ko/javascript/capitalizing-a-string.md
date:                  2024-02-03T19:05:56.250826-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
