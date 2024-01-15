---
title:                "문자열 연결하기"
html_title:           "Elixir: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
문자열을 연결하는 것이 왜 필요한지 궁금해 하셨나요? 문자열 연결은 Elixir 프로그래밍에서 가장 많이 사용되는 기능 중 하나입니다. 문자열을 합치는 것은 다양한 데이터를 하나로 합쳐서 보기 쉽고 가독성 높은 코드를 작성하는 데 도움이 됩니다.

## 어떻게
문자열을 연결하는 방법은 간단합니다. 먼저 `<>` 연산자를 사용하여 문자열을 연결할 수 있습니다. 코드 블록 안의 예제로 살펴보겠습니다.

```elixir
first_string = "안녕하세요,"
second_string = " 저는"
third_string = " Elixir 프로그래머입니다."
concatenated_string = first_string <> second_string <> third_string
IO.puts(concatenated_string)
```

출력 결과는 `안녕하세요, 저는 Elixir 프로그래머입니다.`가 됩니다. 또 다른 방법으로는 `join/2` 함수를 사용하는 것입니다. 이 함수는 문자열 리스트를 하나의 문자열로 연결합니다. 아래 코드를 확인해보세요.

```elixir
string_list = ["여러분은", " 이제", " Elixir 전문가입니다."]
concatenated_string = Enum.join(string_list, "")
IO.puts(concatenated_string)
```

출력 결과는 `여러분은 이제 Elixir 전문가입니다.`가 됩니다.

## 깊이 들어가기
Elixir에서 문자열을 연결할 때 가장 중요한 점은 문자열을 변경할 수 없다는 것입니다. 따라서 새로운 문자열을 만들어야 합니다. 이는 성능 저하를 초래할 수 있으므로 여러 개의 문자열을 연결할 때는 `join/2` 함수를 사용하는 것이 더 효율적입니다.

또한 Elixir에서는 `String` 모듈을 사용하여 문자열을 다룰 수 있습니다. 이 모듈에는 `concat/2` 함수가 포함되어 있어 문자열을 연결할 때 사용할 수 있습니다. `concat/2` 함수는 `join/2` 함수와 비슷하지만 항상 `<>` 연산자와 같은 결과를 출력합니다.

## 참고 자료
- [Elixir String 문서](https://hexdocs.pm/elixir/String.html)
- [Elixir Enum 모듈 문서](https://hexdocs.pm/elixir/Enum.html)