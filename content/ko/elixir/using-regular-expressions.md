---
title:                "Elixir: 정규 표현식 사용하기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하는 이유에 대해 알아보겠습니다. 이 간단한 기술은 문자열 처리에 매우 유용하며 코딩 속도를 대폭 증가시킬 수 있습니다.

## 사용 방법
마크다운과 같은 문자열을 처리해야 할 때, 정규 표현식을 사용하면 코드의 양을 대폭 줄일 수 있습니다. 예를 들어, 특정 패턴을 찾아 대체하는 작업은 간단하게 `Regex.replace(pattern, replacement, target_string)` 함수를 사용하여 처리할 수 있습니다.

```Elixir
Regex.replace(~r/hello/, "hi", "hello world")
=> "hi world"
```

또한, 패턴을 검색하고 추출할 수도 있습니다. 아래의 예시 코드를 살펴보세요.

```Elixir
Regex.scan(~r/[apple|orange]/, "I have an apple and an orange.")
=> [["apple"], ["orange"]]
```

위 예시에서 볼 수 있듯이, 정규 표현식은 여러 개의 결과를 반환할 수 있습니다. 따라서 이를 적절히 활용하면 복잡한 문자열 처리도 쉽게 할 수 있습니다.

## 깊은 곳 파헤치기
정규 표현식은 수십 가지의 메타 문자와 특수한 사용 방법을 가지고 있습니다. 이에 대해 모두 다루는 것은 이 글의 범위를 넘어서므로, 아래의 링크들을 통해 추가 정보를 참고하시길 바랍니다.

## 더 알아보기
- [Elixir 공식 문서 - 정규 표현식](https://elixir-lang.org/getting-started/runes-and-grapheme-clusters.html#regular-expressions)
- [정규 표현식 레퍼런스](https://elixirschool.com/lessons/specifics/regexp)
- [정규 표현식 playground](https://regex101.com/)
- [정규 표현식 게임](https://regexcrossword.com/)