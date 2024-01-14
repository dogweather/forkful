---
title:                "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

*여러분은 왜 검색과 변경 텍스트에 관심을 가질까요?*

수많은 애플리케이션을 작성하다 보면, 텍스트를 검색하고 원하는 형태로 변경하는 일은 흔히 발생하는 일입니다. 이를 자동화하여 시간과 노력을 절약하는 것이 Elixir의 강력한 기능 중 하나입니다. 그래서 여러분도 이번 포스트에서 배워보고 실제 프로젝트에 적용해 보는 것을 추천합니다.

## 왜

Elixir에서는 정규표현식을 사용하여 빠르고 유연하게 텍스트를 검색하고 변경할 수 있습니다. 옵션과 콜백 함수를 사용하여 더 정교한 검색 및 변경 작업을 수행할 수 있으며, 이를 통해 많은 반복적인 작업을 체계적으로 처리할 수 있습니다.

## 사용 방법

검색 및 변경을 위해 Elixir에서는 `Regex` 모듈과 해당 함수를 제공합니다. 간단한 예제를 살펴보겠습니다.

```Elixir
regex = ~r/\d+/  # 정규표현식으로 숫자를 검색합니다.
text = "I have 3 apples and 5 oranges."
Regex.replace(regex, text, "many")  # 결과: "I have many apples and many oranges."
```

위 예제에서 `Regex.replace` 함수는 `text`에서 숫자를 검색하여 `"many"`로 변경합니다. 따라서 `"3"`과 `"5"`가 각각 `"many"`로 변경된 것을 볼 수 있습니다.

더 복잡한 경우, 옵션과 함께 `Regex.replace` 함수를 사용할 수 있습니다. 예를 들어, `count` 옵션을 사용하여 원하는 만큼만 변경하도록 할 수 있습니다.

```Elixir
regex = ~r/cat/  # 정규표현식으로 "cat" 단어를 검색합니다.
text = "I have a cat, a dog, and a cat."
Regex.replace(regex, text, "dog", count: 1)  # 결과: "I have a dog, a dog, and a cat."
```

위 예제에서 `Regex.replace` 함수는 `text`에서 `"cat"` 단어를 `"dog"`로 변경하며, `count: 1` 옵션을 통해 첫 번째로 검색된 단어만 변경하도록 했습니다.

더 자세한 사항은 [Elixir 공식 문서](https://hexdocs.pm/elixir/Regex.html)를 참조하시기 바랍니다.

## 깊이 있는 이해

검색 및 변경에 대해 더 깊이 알아보겠습니다. `Regex` 모듈은 다음과 같은 많은 함수를 제공합니다.

- `match?`: 문자열이 주어진 정규표현식과 일치하는지 확인합니다.
- `scan`: 모든 일치하는 항목을 찾아 리스트로 반환합니다.
- `split`: 일치하는 부분을 기준으로 문자열을 나눕니다.

더 많은 함수와 옵션에 대해 궁금하시다면 [Elixir 공식 문서](https://hexdocs.pm/elixir/Regex.html)를 참조하시기 바랍니다.

See Also

- [정규표현식 기본 배우기](https://www.educative.io/courses/learn-regex)
- [정규표현식으로 텍스트 검색 및 변경하기](https://www.regular-expressions.info/)
- [Elixir에서 정규표현식 사용하기](https://thepowerofcomposition.com/regular-expressions-using-the-elixir-regex-module/)