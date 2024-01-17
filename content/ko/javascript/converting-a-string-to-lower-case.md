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

# 무엇 & 왜?: 문자열 소문자로 변환하는 것은 무엇이며, 프로그래머들이 이렇게 하는 이유는 무엇인가요?

문자열 소문자로 변환(convert)는 문자들을 모두 소문자로 만드는 것을 뜻합니다. 프로그래머들은 문자열을 일관된 형식으로 표현하고 처리하기 위해 이 작업을 수행합니다. 예를 들어, 대문자와 소문자가 섞인 문자열이 있다면, 모두 소문자로 변환하면 문자열을 더 쉽게 분석하고 다룰 수 있습니다.

# 어떻게?: 코딩 예제 및 `Javascript`코드 블록을 사용한 출력 샘플

```Javascript
let str = "HeLLo WoRLd"; // 변환할 문자열
str.toLowerCase(); // 메소드를 사용하여 소문자로 변환
console.log(str); // 출력 결과: "hello world"
```

# 깊이 파고들기: 역사적 배경, 대안 및 구현 세부사항

대소문자 변환은 컴퓨터 프로그래밍에서 매우 일반적인 작업입니다. 대문자와 소문자를 구분하는 철자법이 발전하기 시작한 뒤, 문자열을 일관된 형식으로 표현하기 위해 대소문자를 변환하는 방법이 고안되었습니다.

이 외에도, 대소문자 변환에는 여러 가지 방법이 있으며, 각각의 장단점이 있습니다. `toUpperCase()` 메소드를 사용하면 대문자로 변환할 수 있고, `replace()` 메소드를 사용하면 원하는 문자열을 원하는 대소문자로 변환할 수 있습니다. 하지만 `toLowerCase()` 메소드는 가장 간단하고 일관적인 방법이며, 대부분의 컴퓨터 프로그래밍 언어에서 이 메소드를 제공합니다.

# 관련 정보: 관련 소스 링크

- [MDN Web Docs: String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [w3schools.com: JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)