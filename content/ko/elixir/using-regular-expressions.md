---
title:                "정규식 사용하기"
html_title:           "Elixir: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

정규 표현식을 사용하는 것은 문자열에서 패턴을 매칭하는 것을 의미합니다. 프로그래머들은 이를 통해 더 빠르고 정확한 문자열 매칭을 할 수 있습니다.

## 어떻게:

```Elixir
Regex.match?(~r/hello/, "hello world")
```

위 예제에서는 "hello"라는 패턴과 일치하므로 `true`를 리턴합니다.

```Elixir
Regex.scan(~r/a\w+/, "banana apple")
```

위 예제에서는 문자열에서 "a"로 시작하는 모든 단어를 찾으므로 ["banana", "apple"]을 리턴합니다.

## 깊이 파기:

정규 표현식은 복잡한 패턴을 동적으로 매칭하기 때문에 많은 프로그래머들이 선호하는 방식입니다. Python의 정규 표현식과 비교하여 Elixir의 정규 표현식은 더 강력한 결합 기능을 제공합니다. 

대체 방법으로는 Elixir의 String 모듈도 있지만 정규 표현식은 더 다양한 패턴을 매칭할 수 있습니다. 

Elixir에서는 정규 표현식이 `Regex` 모듈을 사용해 컴파일되고 실행됩니다. `Regex.run/3` 함수는 다양한 매칭 옵션을 제공합니다.

## 참조:

- Elixir 문서: https://hexdocs.pm/elixir/Regex.html
- 정규 표현식 Tutorial: https://regexone.com/