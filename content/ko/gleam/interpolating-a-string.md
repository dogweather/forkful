---
title:                "문자열 보간하기"
html_title:           "Gleam: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Gleam 프로그래밍: 스트링 삽입하기

## What & Why?
스트링 삽입(interpolation)이란 무엇인가요? 이는 쉽게 말해서 문자열 안에 변수나 표현식을 삽입하는 것입니다. 이를 통해 프로그래머는 코드를 더 간결하고 유지보수하기 쉽게 만들 수 있습니다.

## How to:
```Gleam
let name = "Alice";
let age = 25;
let message = `Hi, my name is ${name} and I am ${age} years old.`;
```

위의 예시에서처럼, `` ` ``를 사용하여 문자열을 감싸고 삽입할 변수나 표현식을 `${}` 안에 넣으면 됩니다. 이렇게 하면 Gleam에서 문자열 삽입을 사용할 수 있습니다. 출력 결과는 다음과 같습니다:

```
Hi, my name is Alice and I am 25 years old.
```

## Deep Dive:
스트링 삽입의 역사적 배경은 JavaScript의 문자열 템플릿 리터럴에서 차용되었습니다. 이를테면, 다른 언어에서는 문자열 삽입을 위해 별도의 함수를 사용해야 했지만, JavaScript에서는 템플릿 리터럴을 사용하여 간편하게 삽입할 수 있었습니다.

다른 대안으로는 문자열 포맷팅(Formatting)이 있습니다. 이는 문자열에 변수나 표현식을 삽입하는 것이 아닌, 이를 포맷하여 출력하는 방식입니다. 하지만 이는 복잡한 코드를 만들기 쉽고, 실수로 변수나 표현식의 순서를 바꾸거나 누락할 수 있는 단점이 있습니다.

Gleam에서는 문자열 삽입을 구현하기 위해 특별한 기능이 추가되지 않았습니다. 그 대신, 문자열 삽입을 위한 표준 라이브러리 함수로 `String.sprintf()`가 제공됩니다.

## See Also:
- [JavaScript Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [String Formatting in Python](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)
- [Gleam Documentation on String.sprintf()](https://gleam.run/documentation/stdlib/string#String.sprintf)