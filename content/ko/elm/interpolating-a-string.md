---
title:                "문자열 보간하기"
html_title:           "Elm: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열 보간(interpolating a string)은 Elm에서 가장 유용한 기능 중 하나입니다. 이 기술은 문자열 안에 변수 값을 포함하거나, 특정 문자열을 반복해서 사용하는 등의 작업을 쉽게 할 수 있도록 해줍니다. 프로그래머들은 이 기능을 사용하여 코드를 간결하고 가독성 있게 만들 수 있습니다.

## 하는 방법:
Elm에서 문자열 보간을 하려면, 기존의 문자열 템플릿 기능을 사용하면 됩니다. 아래의 예시를 참고해보세요.

```Elm
name = "John"
age = 25
message = "My name is ${name} and I am ${age} years old."

-- 예상 출력: My name is John and I am 25 years old.
```

또 다른 예시로, 아래와 같이 반복문을 사용하여 특정 문자열을 여러 번 출력할 수 있습니다.

```Elm
repeatString string count =
  String.repeat count string

myString = "hello "
myRepeatedString = repeatString myString 5

-- 예상 출력: hello hello hello hello hello
```

## 깊은 고민:
문자열 보간은 다른 언어에서도 많이 사용되는 기술입니다. 하지만, Elm에서는 이 기능을 더욱 간단하고 안전하게 사용할 수 있도록 지원하고 있습니다. 또한, 문자열 대신 Format 라이브러리를 사용하여 저수준의 문자열 조합 로직을 작성할 수도 있습니다.

## 관련 자료:
- [Elm 공식 문서](https://guide.elm-lang.org/)
- [Elm 문자열 보간 예시](https://ellie-app.com/4JZ9F4kJD3Ma1)
- [Elm Format 라이브러리 문서](https://package.elm-lang.org/packages/elm-community/elm-string-format/latest/)