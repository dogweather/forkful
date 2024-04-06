---
date: 2024-01-20 17:58:06.107996-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C) \uC774\uC804\uC5D0\uB294 '\uBB38\uC790\uC5F4\
  '.replace \uAC19\uC740 \uB2E8\uC21C \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD588\uC9C0\
  \uB9CC, \uB2E8\uC5B4\uC758 \uD328\uD134\uC774\uB098 \uC704\uCE58\uAC00 \uBCF5\uC7A1\
  \uD55C \uACBD\uC6B0 \uC815\uADDC \uD45C\uD604\uC2DD\uC774 \uD544\uC218\uC801\uC785\
  \uB2C8\uB2E4. \uC790\uBC14\uC2A4\uD06C\uB9BD\uD2B8\uB294 ES5\uC5D0 \uC774\uB974\uB7EC\
  \ \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uAC15\uD654\uD558\uBA74\uC11C, \uBB38\uC790\
  \uC5F4 \uCC98\uB9AC \uB2A5\uB825\uC774 \uD06C\uAC8C \uD5A5\uC0C1\uB418\uC5C8\uC2B5\
  \uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.377558-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C) \uC774\uC804\uC5D0\uB294 '\uBB38\uC790\uC5F4'.replace\
  \ \uAC19\uC740 \uB2E8\uC21C \uBA54\uC18C\uB4DC\uB97C \uC0AC\uC6A9\uD588\uC9C0\uB9CC\
  , \uB2E8\uC5B4\uC758 \uD328\uD134\uC774\uB098 \uC704\uCE58\uAC00 \uBCF5\uC7A1\uD55C\
  \ \uACBD\uC6B0 \uC815\uADDC \uD45C\uD604\uC2DD\uC774 \uD544\uC218\uC801\uC785\uB2C8\
  \uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## How to: (어떻게)
```Javascript
// 단순 교체
let str = "안녕하세요, JavaScript입니다!";
let newStr = str.replace("JavaScript", "JS");
console.log(newStr); // "안녕하세요, JS입니다!"

// 정규 표현식을 사용한 전역 교체
let regex = /사과/g; // 'g' 플래그는 전역 검색을 의미
let fruitStr = "사과와 바나나가 있습니다. 사과를 좋아하세요?";
let newFruitStr = fruitStr.replace(regex, "오렌지");
console.log(newFruitStr); // "오렌지와 바나나가 있습니다. 오렌지를 좋아하세요?"
```

## Deep Dive (심층 분석)
이전에는 '문자열'.replace 같은 단순 메소드를 사용했지만, 단어의 패턴이나 위치가 복잡한 경우 정규 표현식이 필수적입니다. 자바스크립트는 ES5에 이르러 정규 표현식을 강화하면서, 문자열 처리 능력이 크게 향상되었습니다. 

`String.prototype.replace()`는 두 파라미터를 취합니다: 검색 패턴(문자열 혹은 정규 표현식)과 교체할 문자열 혹은 함수입니다. 정규 표현식에 'g' 플래그를 사용하지 않으면 첫 번째로 찾은 매치만 교체합니다. 함수를 사용하면 매치된 부분마다 복잡한 로직을 적용할 수 있습니다.

## See Also (참고하기)
- MDN Web Docs: [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- 정규 표현식 가이드: [RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- String methods in JavaScript: [String Methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
