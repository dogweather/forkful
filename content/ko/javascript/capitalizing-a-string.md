---
title:                "문자열 대문자화하기"
html_title:           "Javascript: 문자열 대문자화하기"
simple_title:         "문자열 대문자화하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

이미 알고 있는 사실이지만, 프로그래밍에서 전체적으로 성공적인 결과를 얻기 위해서는 세부적인 작업들이 중요합니다. 대문자로 문자열을 변환하는 것도 그 중 하나입니다. 이 작업은 데이터 처리 등에 매우 유용하며, 성능을 높일 수 있는 간단한 방법입니다.

## 사용 방법
```Javascript
const string = "hello, world";
const capitalizedString = string.toUpperCase();

console.log(capitalizedString);

// Output: HELLO, WORLD
```

각각의 문자열에 대해 `.toUpperCase()`를 사용하여 전체적으로 대문자로 변환할 수 있습니다. 이 방법은 웹 개발 뿐만 아니라 다른 프로그래밍 분야에서도 많이 사용되는 기법입니다.

## 깊이 파고들기

이 방법을 사용하여 문자열을 대문자로 변환하면 실제로는 문자열 내의 모든 문자를 대문자로 변환하는 것이 아닙니다. 대신, 해당 문자열을 원본 문자열의 복제본으로 변환하고 각 문자를 대문자로 변환한 다음 새 문자열로 반환합니다. 이는 원본 문자열을 변경하지 않고 대문자로 변환된 문자열을 사용하게 됩니다.

## 더 알아보기

- [MDN Web Docs: String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [W3Schools: JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)