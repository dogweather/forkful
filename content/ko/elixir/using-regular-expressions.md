---
title:                "Elixir: 정규 표현식을 사용하는 방법"
simple_title:         "정규 표현식을 사용하는 방법"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜 정규 표현식을 사용해야 할까요?
짧게 말하자면, 정규 표현식은 문자열의 패턴을 정의하는 강력하고 유연한 도구입니다. 이는 Elixir에서 개발자들이 특정한 패턴을 찾거나 검증하는 것을 도와줍니다.

## 사용 방법
먼저, 정규 표현식이라는 용어가 생소할 수 있습니다. 정규 표현식은 특정한 패턴을 찾거나 이해하는 데 사용되는 문자열입니다. 특정 패턴을 찾기 위해서는 `Regex` 모듈의 `match?` 함수를 사용합니다.

```elixir
Regex.match?("Hello, world!", ~r/world/) #=> true
Regex.match?("Goodbye, world!", ~r/hello/) #=> false
```

또 다른 예제로, 이메일 주소의 유효성을 확인해 보겠습니다.

```elixir
Regex.match?("hello@example.com", ~r/\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/) #=> true
Regex.match?("helloexample.com", ~r/\A[\w+\-.]+@[a-z\d\-.]+\.[a-z]+\z/) #=> false
```

## 깊이 파고들기
정규 표현식은 다양한 패턴을 찾는 데 사용할 수 있습니다. 예를 들어, `Regex.scan` 함수를 사용하면 문자열에서 특정 패턴을 모두 찾아서 리스트로 반환할 수 있습니다. 또는 `Regex.split` 함수를 사용하여 문자열을 특정 패턴을 기준으로 나눌 수도 있습니다.

```elixir
Regex.scan("Hello, John! How are you?", ~r/[a-z]+/) #=> ["Hello", "John", "How", "are", "you"]
Regex.split("apple,banana,orange", ~r/,/) #=> ["apple", "banana", "orange"]
```

정규 표현식을 사용할 때는 패턴을 제대로 이해하는 것이 중요합니다. 이를 위해서는 `Regex.escape` 함수를 사용하여 특수 문자를 이스케이핑해야 합니다.

```elixir
Regex.escape("Hello, [world]") #=> "Hello, \\[world\\]"
```

더 많은 패턴 및 함수를 알아보려면 Elixir 공식 문서를 참조해 주세요.

# 방문해 보세요
- Elixir 공식 문서: https://hexdocs.pm/elixir/Regex.html
- 정규 표현식 테스트: https://regex101.com/r/E2ILsY/1
- Elixir 커뮤니티: https://elixirforum.com/