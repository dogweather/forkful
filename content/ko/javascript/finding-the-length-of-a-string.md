---
title:                "문자열의 길이 찾기"
html_title:           "Lua: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열의 길이를 찾는 것은 문자열에 포함된 문자 수를 세는 것입니다. 이는 문자열 조작, 검증, 그리고 데이터 처리와 같은 다양한 프로그래밍 상황에서 중요한 작업입니다. 

## 어떻게 사용하는가:

```Javascript
let str = "안녕하세요";
console.log(str.length); // 출력: 5
```

이 코드에서 `str.length`는 우리가 센 문자의 수를 반환합니다. 이 경우, "안녕하세요"라는 문자열에는 5개의 문자가 있으므로 5가 반환됩니다.

## 깊이 들여다보기:

### 역사적 맥락:

Javascript에서 문자열 길이를 찾는 방법은 ECMA-262 표준에서 제대로 정의되었으며, 이는 자바스크립트의 개발 초기부터 존재했습니다.

### 대체 방법 :

대부분의 경우, `length` 속성을 사용하는 것이 최선의 방법입니다. 그러나 구식 브라우저에서는 array-like object를 사용하여 동일한 결과를 얻을 수 있습니다. 그러나 이 방법은 보통 권장되지 않습니다. 

```javascript
let str = "안녕하세요";
let length = Array.prototype.slice.call(str).length;
console.log(length); // 출력: 5
```

### 구현 세부 내용:

`length` 속성은 문자열 내에서 실제 바이트 수에 관계없이 문자 수를 반환합니다. 주의 깊게 살펴보면, 이는 Unicode와 관련된 사항일 수 있습니다. 

## 또한 참조하십시오 :

1. [Mozilla Developer Network's Reference on 'String.length'](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/length)
2. [W3Schools' Explanation of JavaScript String Length Property](https://www.w3schools.com/jsref/jsref_length_string.asp)