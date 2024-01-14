---
title:                "Elixir: 문자열 추출하기"
simple_title:         "문자열 추출하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

자바에서 Elixir로 전환하면서 문자열 추출에 대한 방법이 달라졌음을 알게 되었습니다. 이를 알기 전까지는 일반적인 방법만을 알고 있었기 때문에, Elixir에서 제공하는 다양하고 강력한 문자열 추출 기능에 대해 알아보고자 합니다.

## 하나의 방법

일반적인 문자열 추출 방법은 다음과 같습니다.

```Elixir
name = "홍길동"
substring = String.slice(name, 0, 2)
IO.puts(substring)
```

출력 결과는 "홍길"이 됩니다. 하지만 Elixir에서는 더욱 간단하게 문자열을 추출할 수 있습니다.

```Elixir
name = "홍길동"
substring = name[0..1]
IO.puts(substring)
```

위의 코드를 실행하면 동일한 결과인 "홍길"이 나옵니다. 이 방법은 직관적이고 쉽게 이해할 수 있기 때문에 추천합니다.

## 깊이 파보기

Elixir에서는 다양한 방법으로 문자열을 추출할 수 있습니다. 위에서 소개한 `[0..1]` 외에도 `String.substring/3`, `String.substring/2`, `String.slice/2`, `String.split/2` 등의 함수를 사용할 수 있습니다. 각 함수의 기능과 사용법을 익히면 더욱 효율적인 문자열 추출이 가능해집니다.

## 관련 링크

- [Elixir 공식 문서](https://hexdocs.pm/elixir/String.html)
- [Elixir 문자열 추출 예제](https://elixir-lang.org/getting-started/modules#string-slice-or-substring)
- [Elixir 문자열 함수들과 사용 예제](https://elixir-lang.org/crash-course.html#string-manipulation)
- [Elixir 문자열 추출 실습](https://elixirschool.com/lessons/basics/string-manipulation/)