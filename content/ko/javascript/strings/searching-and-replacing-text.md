---
date: 2024-01-20 17:58:06.107996-07:00
description: "\uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\uCCB4\uD558\
  \uB294 \uAC83\uC740 \uBB38\uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uBB38\uC790\uB098\
  \ \uB2E8\uC5B4\uB97C \uCC3E\uC544 \uBCC0\uACBD\uD558\uB294 \uACFC\uC815\uC785\uB2C8\
  \uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130 \uC815\uC81C, \uD328\
  \uD134 \uB9E4\uCE6D, \uC0AC\uC6A9\uC790 \uC785\uB825 \uBCC0\uD658 \uB4F1\uC758 \uC791\
  \uC5C5\uC744 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.770613-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8\uB97C \uAC80\uC0C9\uD558\uACE0 \uAD50\uCCB4\uD558\uB294\
  \ \uAC83\uC740 \uBB38\uC790\uC5F4\uC5D0\uC11C \uD2B9\uC815 \uBB38\uC790\uB098 \uB2E8\
  \uC5B4\uB97C \uCC3E\uC544 \uBCC0\uACBD\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130 \uC815\uC81C, \uD328\uD134\
  \ \uB9E4\uCE6D, \uC0AC\uC6A9\uC790 \uC785\uB825 \uBCC0\uD658 \uB4F1\uC758 \uC791\
  \uC5C5\uC744 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  ."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## What & Why? (무엇과 왜?)
텍스트를 검색하고 교체하는 것은 문자열에서 특정 문자나 단어를 찾아 변경하는 과정입니다. 프로그래머는 데이터 정제, 패턴 매칭, 사용자 입력 변환 등의 작업을 위해 이 기능을 사용합니다.

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
