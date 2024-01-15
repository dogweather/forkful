---
title:                "문자열을 소문자로 변환하기"
html_title:           "Javascript: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 왜: 문자열을 소문자로 변환하는 데 참여하는 이유

문자열을 모두 대문자로 쓰는 것보다 작업이 더 쉽고 편리하기 때문입니다. 예를 들어, 사용자 이름을 검사할 때 대소문자를 구분하지 않는 경우, 입력한 값이 소문자로 변환되는 것이 좋습니다.

## 작업 방법

```Javascript
const userInput = "Hello WORLD";
const lowerCaseInput = userInput.toLowerCase();

console.log(lowerCaseInput);

// Output: hello world
```

위의 예시에서는 `toLowerCase()` 메서드를 사용하여 사용자로부터 입력받은 문자열을 소문자로 변환하고, 변환된 값을 출력하고 있습니다. 이렇게 하면 기존의 대문자로 된 값이 소문자로 변환되어 출력됩니다.

## 깊게 알아보기

`toLowerCase()` 메서드는 문자열을 소문자로 변환하는 일은 물론, 특정 언어의 규칙에 따라 변환하는 데도 사용될 수 있습니다. 예를 들어 터키어의 경우 `toLowerCase()` 메서드를 사용하여 `I` 대신 `ı`, `İ` 대신 `i`를 출력할 수 있습니다.

또한 `toLowerCase()` 메서드는 상황에 따라 원본 문자열을 수정하거나(이를peller로 전달한 경우), 새로운 문자열을 반환합니다. 따라서 사용자가 이 메서드를 적용하는 상황에 따라 적절한 방식을 선택해야 합니다.

## 또 다른 이것을 읽어보세요

- [Javascript 문자열 메서드 참조](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)
- [Javascript 대소문자 변환 방법](https://www.w3schools.com/jsref/jsref_toLowercase.asp)