---
title:    "Elixir: 패턴과 일치하는 문자 제거하기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 왜
## 왜 문자열에서 패턴에 맞는 문자를 삭제하는 것이 유용할까요?
문자열 데이터에서 불필요한 문자를 삭제하는 것은 데이터의 가독성을 높이고, 데이터 처리 및 분석을 용이하게 만드는 중요한 작업입니다. 이를 통해 데이터의 정제와 수정을 효율적으로 수행할 수 있습니다.

# 방법
## Elixir를 사용하여 문자열에서 패턴에 맞는 문자 삭제하기
패턴 매칭 기능을 제공하는 Elixir는 간단한 코드로 문자열에서 패턴에 맞는 문자를 삭제할 수 있습니다. 아래는 예시 코드와 결과입니다.

```Elixir
str = "Hello, World!"
new_str = String.replace(str, ~r/o/, "") # "Hell, Wrld!"
```

위의 코드에서 우리는 `String.replace/3` 함수를 사용하여 문자열에서 `o`라는 문자를 삭제했습니다. 이와 같은 방법으로 다양한 패턴에 대해서도 삭제할 수 있으며, 더 많은 정보는 Elixir 공식 문서를 참고해주세요.

# 깊게 파고들기
## 패턴 매칭을 사용한 문자열 수정의 원리
Elixir의 `String.replace/3` 함수는 내부적으로 정규표현식을 사용하여 패턴 매칭을 수행합니다. 이를 통해 우리는 정교한 문자열 수정을 할 수 있습니다. 예를 들어, 아래 코드에서는 `i`로 시작하는 단어만 삭제하고 나머지 단어를 유지하는 패턴을 적용하였습니다.

```Elixir
str = "I like to eat apples and bananas."
new_str = String.replace(str, ~r/i\w+\b/, "")
# "like to eat and"
```

이처럼 정규표현식을 이용하면, 우리는 더욱 복잡한 패턴에 대해서도 문자열을 수정할 수 있습니다.

# 관련 링크
## 참고할 만한 다른 링크들

- [Elixir 공식 문서](https://elixir-lang.org/getting-started/pattern-matching.html)
- [정규표현식에 대한 기초 지식](https://www.rexegg.com/regex-quickstart.html)
- [Elixir 내장 함수에 대한 더 자세한 정보](https://hexdocs.pm/elixir/String.html#replace/4)