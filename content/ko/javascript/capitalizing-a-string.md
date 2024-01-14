---
title:    "Javascript: 문자열 대문자로 변환하기"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 왜

자바스크립트에서 문자열의 첫글자만 대문자로 바꾸는 것은 간단한 작업이지만, 코드의 가독성을 높이는 데 매우 중요합니다.

## 방법

```Javascript
// 전달받은 문자열의 첫글자를 대문자로 변경하는 함수
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

let input = "javascript";
let output = capitalize(input);

console.log(output); // "Javascript"
```

## 깊은 이해

자바스크립트에서는 문자열의 각 글자를 각각 다뤄야 하기 때문에, 첫글자를 대문자로 변경하는 것은 간단하지 않습니다. `str.charAt(0)`를 사용하여 첫글자를 가져온 후 `.toUpperCase()`를 사용하여 대문자로 바꾸고, 나머지 글자는 `.slice(1)`을 사용하여 원래 문자열과 결합하여 대문자가 된 첫글자를 포함한 새로운 문자열을 반환합니다.

## 관련 자료

[자바스크립트 문자열 다루기](https://poiemaweb.com/js-string) <br>
[자바스크립트 함수(Function)](https://www.w3schools.com/js/js_functions.asp)

## 참고 자료

메인 디자인 레퍼런스: [Markdown 가이드](https://www.markdownguide.org/) <br>
코드 블록 가이드: [GitHub Markdown Syntax](https://guides.github.com/features/mastering-markdown/)