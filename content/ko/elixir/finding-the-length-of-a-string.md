---
title:    "Elixir: 문자열의 길이 찾기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것에 대해 관심을 가질 이유는 여러 가지가 있습니다. 예를 들어, 사용자 입력을 검증하거나 문자열 처리를 위해 필요할 수 있습니다.

## 어떻게

문자열의 길이를 찾는 가장 간단한 방법은 Elixir 내장 함수인 `String.length()`를 사용하는 것입니다. 이 함수는 문자열의 길이를 정수로 반환합니다.

```Elixir
text = "안녕하세요!"
String.length(text)
# 출력: 6
```

또 다른 방법은 문자열을 리스트로 변환한 후 `length()` 함수를 사용하는 것입니다. 이 경우에는 띄어쓰기도 한 글자로 카운트되므로 조심해야 합니다.

```Elixir
text = "Hello, world!"
text |> String.graphemes() |> length()
# 출력: 13
```

## 깊게 들어가기

문자열의 길이를 어떻게 정의할 수 있을까요? 실제로는 문자열이 저장된 방식에 따라 달라질 수 있습니다. Elixir에서는 문자열을 유니코드 코드 포인트의 리스트로 표현합니다. 따라서 `String.length()` 함수는 리스트의 길이를 반환합니다.

만약, 한글과 같이 다중 바이트 문자를 사용하는 문자열을 다룬다면, `String.length()`의 결과가 예상과 다를 수 있습니다. 이를 방지하기 위해서는 `String.grapheme_length()` 함수를 사용하여 문자 그래프의 수를 반환해야 합니다.

## 참고 자료

- [Elixir 문자열 관련 문서](https://hexdocs.pm/elixir/String.html#functions)
- [유니코드 그래프의 정의](https://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules)