---
title:                "Elixir: 텍스트 검색 및 교체하기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

"왜: 텍스트를 검색하고 교체하는 작업을 수행하는 이유는 코드의 유지 보수와 간결성을 유지하고 잘못된 데이터를 수정하기 위해서입니다.

## 왜

코드를 작성하다보면 때로는 특정 문자열을 검색하고 그에 해당하는 부분을 다른 문자열로 교체해야 할 때가 있습니다. 이런 작업은 코드를 보다 간결하고 유지 보수가 용이하게 만들어주며, 잘못된 데이터를 수정할 수 있는 유용한 방법입니다.

## 어떻게

Elixir에서는 `Enum.replace/4` 함수를 사용하여 특정 문자열을 검색하고 다른 문자열로 교체할 수 있습니다. 예를 들어, 다음과 같은 코드를 작성할 수 있습니다.

```Elixir
text = "안녕하세요, 저는 Elixir를 배우고 있습니다."
result = Enum.replace(text, "Elixir", "파이썬")
IO.puts(result)

# 출력 결과: "안녕하세요, 저는 파이썬를 배우고 있습니다."
```

검색 대상이 되는 문자열이 여러 개일 경우, 다른 예시 코드를 살펴봅시다.

```Elixir
text = "Elixir는 함수형 프로그래밍 언어입니다. Elixir를 배우면 새로운 세계를 경험할 수 있습니다."
result = Enum.replace(text, "Elixir", "파이썬", global: true)
IO.puts(result)

# 출력 결과: "파이썬는 함수형 프로그래밍 언어입니다. 파이썬를 배우면 새로운 세계를 경험할 수 있습니다."
```

위의 예시에서 `global` 옵션을 사용하면 모든 검색 대상을 변경할 수 있습니다.

## 심층 분석

`Enum.replace/4` 함수는 주어진 문자열에서 특정 문자열을 검색하고 다른 문자열로 교체하는 기능을 하지만 더 복잡한 작업을 수행하기 위해서는 정규 표현식을 사용해야 할 수도 있습니다. Elixir에서는 `Regex` 모듈을 사용하여 정규 표현식을 작성하고 이를 `String.replace/3` 함수에 적용할 수 있습니다.

```Elixir
text = "abc123 456efg"
result = String.replace(text, ~r/\d+/, "XYZ")
IO.puts(result)

# 출력 결과: "abcXYZ 456efg"
```

위의 예시에서는 숫자를 검색하고 `XYZ`로 교체했지만, 조건을 더 추가하여 더 복잡한 검색과 교체 작업을 수행할 수 있습니다. 자세한 내용은 Elixir 문서를 참고해주세요.

## 참고 자료

- [Elixir 문서](https://elixir-lang.org/getting-started/regex.html)
- [Elixir Enum 모듈](https://hexdocs.pm/elixir/Enum.html)
- [Elixir Regex 모듈](https://hexdocs.pm/elixir/Regex.html)

## 참고 자료

- [Elixir 문서](https://elixir-lang.org/getting-started/regex.html)
- [Elixir Enum 모듈](https://hexdocs.pm/elixir/Enum.html)
- [Elixir Regex 모듈](https://hexdocs.pm/elixir/Regex.html)