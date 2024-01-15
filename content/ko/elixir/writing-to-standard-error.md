---
title:                "표준 오류에 쓰는 방법"
html_title:           "Elixir: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

가장 일반적인 디버깅 방법 중 하나는 상태나 오류 메시지를 콘솔에 출력하는 것입니다. 이는 작은 오류를 신속하게 발견하고 관련된 정보를 확인하는 데에 매우 유용합니다.

## 어떻게

Elixir에서는 `IO.puts/2` 함수를 사용하여 텍스트를 표준 출력으로 출력할 수 있지만, 오류 메시지는 일반적으로 표준 에러에 출력되어야 합니다. 이를 위해서는 `IO.puts/2` 대신 표준 에러를 처리하는 `IO.puts/3` 함수를 사용해야 합니다. 아래는 간단한 예제 코드입니다.

```Elixir
IO.puts("표준 출력")
IO.puts(:stderr, "표준 에러")
```

이 코드는 다음과 같이 출력됩니다.

```
표준 출력
표준 에러
```

## 깊이 파고들기

표준 에러는 대개 로그 파일이나 관리 콘솔과 같은 다른 장소에 저장되어 실제로 접근할 수 있게 됩니다. 또한, `Logger` 모듈을 사용하여 더 정교한 로깅 방식을 구현할 수도 있습니다. 이 모듈을 사용하면 로그를 원하는 위치에 기록하고, 로그 레벨을 지정하거나 필터링하여 더 유용한 정보를 얻을 수 있습니다.

## 참고 자료

- [Elixir 공식 문서](https://hexdocs.pm/elixir/1.12/IO.html#puts/3)
- [Elixir School](https://elixirschool.com/ko/lessons/basics/io-and-the-file-system/)
- [Elixir 프로그래밍 정우영](https://programmer.ink/think/using-logger-in-elixir.html)