---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

정규 표현식은 텍스트를 검색하고 수정하는데 사용되는 패턴입니다. 프로그래머는 텍스트 데이터를 효율적이고 정확하게 처리하기 위해 이를 활용합니다.

## 어떻게 사용하나요:

Elixir에서 정규 표현식을 사용하는 방법은 쉽습니다. 아래 코드를 살펴봅시다.

```Elixir
  string = "Hello, Elixir programmers!"
  Regex.scan(~r/[A-Z][a-z]+/, string)
```

이 코드의 출력값은 다음과 같습니다.

```Elixir
  [["Hello"], ["Elixir"]]
```

위의 코드는 대문자로 시작하는 단어를 찾는 정규 표현식입니다.

## 깊게 보기:

정규 표현식 체계는 먼저 1950년대에 개발되었습니다. Elixir에서 정규 표현식을 사용하면, 텍스트에서 특정 패턴을 효율적으로 찾고 대체할 수 있습니다. 그러나 사용자가 잘못 사용하면 코드의 가독성을 떨어뜨릴 가능성이 있으므로 주의가 필요합니다. Elixir의 `String` 모듈 안에는 정규 표현식 없이도 텍스트를 쉽게 처리할 수 있는 함수들이 많습니다.

## 참고 자료:

* Elixir 정규 표현식 공식 문서: [link](https://hexdocs.pm/elixir/1.12/Regex.html)
* Elixir `String` 모듈 공식 문서: [link](https://hexdocs.pm/elixir/1.12/String.html)