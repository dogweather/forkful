---
title:                "문자열 대문자화"
html_title:           "Elixir: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
문자열의 첫 글자를 대문자로 변환하는 것을 문자열 대문자화 라고 합니다. 이 기능은 대소문자를 구분하는 프로그래밍 언어에서 많이 사용됩니다. 가독성을 높이고, 변수나 함수명을 명확하게 하기 위해서 이 기능을 사용해야 합니다.

## 방법:
문자열 대문자화 기능을 사용하기 위해서는 문자열 모듈의 capitalize 함수를 사용하면 간단하게 구현할 수 있습니다. 적절한 매개변수를 전달하면 문자열의 첫 글자가 대문자로 변환됩니다. 예를 들어, 다음과 같이 코드를 작성할 수 있습니다.

```Elixir
name = "jennifer"
capitalize(name)
```

위 코드의 출력 결과는 다음과 같이 나타납니다.

```Elixir
"Jennifer"
```

## 깊이있는 공부:
이 기능은 대소문자를 구분하는 프로그래밍 언어에서 자주 사용됩니다. 예를 들어, C언어에서는 변수명에 대소문자를 구분하여 사용해야 합니다. 또한 대부분의 프로그래밍 언어에서는 변수나 함수명을 소문자로 작성하는 것이 관례입니다. 따라서 이 기능은 코드의 가독성을 높이고, 변수나 함수명을 명확하게 할 수 있습니다.

또한, 문자열 대문자화 기능은 문자열을 다루는 다양한 함수들 중 하나일 뿐입니다. 예를 들어, 대소문자를 구분하지 않는 비교 함수도 있습니다. 따라서 문자열 대문자화 외에도 다양한 방법으로 문자열을 다룰 수 있으며, 적절한 상황에 맞게 선택하여 사용해야 합니다.

문자열 대문자화 기능의 구현 방법은 각 프로그래밍 언어마다 다를 수 있지만, 기본적으로 문자열의 첫 글자를 대문자로 변환하는 로직을 포함하고 있습니다. 이를 활용하여 직접 구현해 볼 수도 있습니다.

## 관련 자료:
- [Elixir 문자열 모듈 공식 문서](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [많이 사용되는 프로그래밍 언어의 대소문자 관련 규칙](https://en.wikipedia.org/wiki/Letter_case#Programming_languages)