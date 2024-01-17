---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Elixir: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

패턴에 일치하는 문자를 삭제한다는 것은 문자열에서 특정 패턴을 찾아 해당하는 문자를 삭제하는 것을 말합니다. 이를 프로그래머들은 주로 문자열 처리 과정에서 특정 문자를 제거하기 위해 사용합니다.

# 하는 법:

```Elixir
String.replace("Hello, World!", ~r/[aeiou]/, "")
```
>Hll, Wrld!

위의 코드는 "Hello, World!" 문자열에서 모음(aeiou)에 해당하는 문자를 삭제하여 "Hll, Wrld!"라는 새로운 문자열을 반환합니다. 이를 위해 Elixir 내장 함수인 `String.replace`를 사용하였으며, 정규 표현식 `~r/[aeiou]/`을 이용하여 패턴을 지정하였습니다.

# 깊이 들어가기:

1. 역사적인 맥락: 문자열 처리는 프로그래밍에서 매우 중요한 요소입니다. 따라서 문자열에서 특정 패턴을 찾아 삭제하는 기능은 예전부터 프로그래머들에게 필요한 기능으로 여겨져 왔습니다.

2. 대안: Elixir에서 문자열을 처리하는 다양한 방법이 존재합니다. 따라서 패턴 삭제에 대해서도 다양한 방법을 사용할 수 있지만, `String.replace` 함수를 사용하는 것이 가장 간편하고 효율적입니다.

3. 구현 상세: Elixir의 `String.replace` 함수는 반복문을 통해 문자열을 순회하면서 해당하는 패턴을 발견하고, 해당하는 문자를 삭제하는 방식으로 구현되어 있습니다.

# 참고 자료:

- [Elixir Documentation: String.replace](https://hexdocs.pm/elixir/String.html#replace/3)
- [Elixir School: Strings](https://elixirschool.com/ko/lessons/basics/basics/#strings)
- [Elixir Forum: Remove All Non-Alphanumeric.](https://elixirforum.com/t/remove-all-non-alphanumeric/24489)