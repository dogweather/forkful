---
title:                "표준 에러에 쓰는 방법"
html_title:           "Elixir: 표준 에러에 쓰는 방법"
simple_title:         "표준 에러에 쓰는 방법"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 그리고 왜?

표준 에러에 쓰는 것은 프로그래머들이 에러 메시지를 출력하기 위해 사용하는 방법입니다. 이는 디버깅과 코드 흐름을 분석하는 데 중요한 도구입니다.

## 하는 방법 :

표준 에러에 쓰는 방법은 아주 쉽습니다. 단지 `IO.puts/2`를 사용하고 에러 메시지를 나타내려는 문자열과 함께 `:stderr` 옵션을 전달하면 됩니다.

```Elixir
IO.puts("에러 메시지입니다.", stderr: :stderr)
```

위의 코드는 다음과 같은 결과를 출력합니다.

```Elixir
에러 메시지입니다.
```

## 깊이 파고들기 :

표준 에러에 쓰는 아이디어는 오래 전부터 존재하였습니다. 하지만 요즘에는 더 다양한 방법으로 에러 메시지를 출력할 수 있습니다. 예를 들어, `IO.inspect/2` 함수를 사용하여 구조화된 데이터를 출력할 수 있습니다. 또한 복잡한 로그 시스템을 사용하여 에러를 추적할 수도 있습니다.

## 관련 자료 :

[Elixir 공식 문서](https://hexdocs.pm/elixir/IO.html#puts/2)에서 `IO.puts/2` 함수에 대해 더 자세한 정보를 확인할 수 있습니다. 또한 [이 블로그 포스트](https://medium.com/@williamhayek/the-art-of-writing-to-stderr-in-elixir-e7c6797eaeac)에서 더 많은 정보를 얻을 수 있습니다.