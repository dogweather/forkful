---
title:                "텍스트 검색 및 대체"
html_title:           "Elixir: 텍스트 검색 및 대체"
simple_title:         "텍스트 검색 및 대체"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇을 & 왜?: (1) 텍스트 찾기 및 대체가 무엇인지, 그리고 (2) 프로그래머들이 왜 그것을 하는지에 대한 설명

텍스트 찾기 및 대체란 특정 문자열을 검색하여 새로운 문자열로 대체하는 작업입니다. 이 작업은 프로그래머들이 자주 하는 작업 중 하나입니다. 우리가 작성한 코드에 예상치 못한 버그가 발생할 때, 특정 문자열을 한 번에 모두 수정하고 싶을 때, 혹은 간단한 오타를 수정할 때 텍스트 찾기 및 대체는 유용한 기능입니다.

## 방법:

1. **정규식을 사용한 검색 및 대체**
  - 정규식을 사용하면 특정 패턴에 맞는 문자열을 쉽게 찾아낼 수 있습니다. 예를 들어 `~r"Hello, (.*)!"` 라는 정규식을 사용하면 "Hello, John!"이라는 문자열에서 "John"이라는 부분을 추출할 수 있습니다. 또한 정규식을 사용하여 문자열을 대체하는 것도 가능합니다. 예를 들어 `Regex.replace(~r"Hello, (.*)!", "Hello, Alice!", "Hello, John!")` 라는 코드를 실행하면 "Hello, John!"이라는 문자열이 "Hello, Alice!"로 변경됩니다.
2. **String.replace를 사용한 대체**
  - 정규식을 사용하지 않고도 특정 문자열을 찾아서 대체할 수 있습니다. `String.replace("Hello, John!", "Hello", "Hi")` 라는 코드를 실행하면 "Hello, John!"이라는 문자열에서 "Hello"를 "Hi"로 변경할 수 있습니다.

## 깊이있게 알아보기:

1. **역사적 배경**
  - 텍스트 찾기 및 대체는 컴퓨터 프로그래밍의 초기부터 사용되어온 기능 중 하나입니다. 초창기의 컴퓨터는 기본적으로 일련의 문자로 구성되어있었기 때문에 텍스트를 찾아서 수정하는 것이 매우 중요했습니다. 그러나 현재는 정규식과 같은 고급기법을 사용하여 더욱 정교한 텍스트 처리를 할 수 있게 되었습니다.
2. **대체 방법**
  - 정규식과 String.replace를 이용한 방법 이외에도 다양한 대체 방법이 존재합니다. 각 방법마다 장단점이 있으므로 상황에 맞게 선택하여 사용해야 합니다. 예를 들어 정규식은 매우 강력하지만 다루기 어렵고 복잡할 수 있습니다.
3. **구현 방법**
  - 텍스트 찾기 및 대체는 대부분의 프로그래밍 언어에서 지원하는 기능이므로 구현 방법은 언어에 따라 다릅니다. Elixir에서는 정규식을 사용하는 `Regex` 모듈과 `String.replace` 함수를 제공하며, 이를 이용하여 텍스트를 검색하고 대체할 수 있습니다.

## 관련 자료:

- [Regex cheatsheet](https://www.rexegg.com/regex-quickstart.html): 정규식에 대한 자세한 설명과 예제를 포함한 치트시트입니다.
- [Elixir String module documentation](https://hexdocs.pm/elixir/String.html): Elixir에서 문자열을 다루는 다양한 함수들을 확인할 수 있습니다.