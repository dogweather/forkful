---
title:                "텍스트 검색 및 바꾸기"
html_title:           "Javascript: 텍스트 검색 및 바꾸기"
simple_title:         "텍스트 검색 및 바꾸기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트에서 텍스트를 검색하고 바꾸는 일에 관심을 가질 수 있는 이유는 우리가 거의 매일 컴퓨터를 사용하면서 텍스트를 작성하고 편집하기 때문입니다.

## 사용 방법

자바스크립트에는 문자열 내에서 원하는 부분을 찾아서 새로운 문자열로 바꿀 수 있는 강력한 기능인 '검색 및 치환'이 내장되어 있습니다. 다음은 코드 예제와 결과를 포함한 사용 방법입니다.

```Javascript
// 문자열에서 '바나나'를 찾아서 '사과'로 바꾸기
let fruits = '오늘은 바나나를 먹겠어요.';
let newFruits = fruits.replace('바나나', '사과');

console.log(newFruits); // '오늘은 사과를 먹겠어요.'
```

## 깊이 있는 탐구

자바스크립트에서 문자열을 검색하고 바꾸는 기능에 대해 더욱 깊이 있는 정보를 알고 싶다면 정규식(Regular Expression)에 대해 학습해보세요. 정규식은 문자열에 대한 패턴을 정의하여 보다 유연한 검색 및 치환 작업을 수행할 수 있도록 해줍니다.

## 참고 자료

- [MDN 문서: String.prototype.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [정규식 테스트 사이트](https://regexr.com/)