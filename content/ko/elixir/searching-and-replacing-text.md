---
title:    "Elixir: 텍스트 검색 및 교체"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

문자열을 검색하고 대체하는 과정은 프로그래밍에서 매우 중요합니다. 예를 들어, 많은 텍스트 파일을 가지고 작업하고 있다고 가정해보겠습니다. 이 파일들은 모두 동일한 단어를 포함하고 있지만, 단어의 철자가 잘못된 경우가 있습니다. 이럴 때 매우 유용하게 사용되는 것이 바로 문자열 검색 및 대체 기능입니다.

# 어떻게 하나요

Elixir에서 문자열을 검색하고 대체하는 방법은 매우 간단합니다. 먼저, `String.replace/4` 함수를 사용합니다. 이 함수는 총 4개의 인수를 받습니다. 첫 번째 인수는 원본 문자열, 두 번째 인수는 검색할 대상 문자열, 세 번째 인수는 대체할 문자열, 마지막 인수는 대소문자를 구분할지 여부를 나타내는 옵션입니다.

```Elixir
iex> String.replace("Hello, World!", "Hello", "Hi")
"Hi, World!"
```

이렇게 하면 간단하게 문자열의 일부분을 다른 문자열로 대체할 수 있습니다.

또한, 정규 표현식을 사용하여 더 다양한 문자열 검색 및 대체 작업을 수행할 수 있습니다. `String.replace/4` 함수 대신 `String.replace/3` 함수를 사용하면 됩니다.

```Elixir
iex> String.replace("Hello, World!", ~r/[a-z]*/, "Code")
"Code, Code!"
```

이렇게 하면 원본 문자열에서 알파벳 소문자가 있는 부분을 모두 "Code"로 대체할 수 있습니다.

# 깊이 파고들기

검색 및 대체 기능에서 더욱 흥미로운 점은 정규 표현식을 사용할 때의 유연성입니다. 예를 들어, 다음과 같은 문자열을 검색해야 한다고 가정해보겠습니다.

```
Hello, ~name~! Welcome to ~city~.
```

여기서 `~`로 둘러싸인 부분은 원하는 내용으로 대체하고 싶은 부분입니다. 이럴 때 유용하게 사용할 수 있는 정규 표현식은 다음과 같습니다.

```regex
~(.+)~
```

이 정규 표현식을 사용하면 `name`과 `city`라는 문자열을 쉽게 추출할 수 있습니다. 이를 `String.replace/3` 함수와 함께 사용하면 다음과 같은 결과를 얻을 수 있습니다.

```Elixir
iex> String.replace("Hello, ~name~! Welcome to ~city~.", ~r/~(.+)~/, "John")
"Hello, John! Welcome to ~city~."
```

여기서 `~city~`라는 문자열은 그대로 유지되고, `~name~`은 `John`으로 대체되었습니다. 이런 방법을 사용하면 매우 간단하게 원하는 문자열을 추출하고 대체할 수 있습니다.

# 또한 보기

- [Elixir 문서 - String 모듈](https://hexdocs.pm/elixir/String.html)
- [정규 표현식 테스트 사이트](https://regexr.com/)