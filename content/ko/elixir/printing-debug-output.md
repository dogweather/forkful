---
title:                "디버그 출력을 인쇄하기"
html_title:           "Clojure: 디버그 출력을 인쇄하기"
simple_title:         "디버그 출력을 인쇄하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

디버그 출력이란 프로그래밍 중에 중간 결과물을 확인하거나 오류를 진단하기 위해 코드의 실행 흐름을 출력하는 것을 말합니다. 프로그래머가 이를 사용하는 주된 이유는 코드에서 오류를 찾거나 프로그램의 작동 방식을 이해하기 용이하게 하기 위함입니다.

## 이렇게 사용합니다:

Elixir에서 디버그 출력을 하는 방법은 IO.inspect를 사용하는 것입니다. 이것은 값을 출력하고 그 값을 반환합니다, 따라서 표현식 안에서 사용할 수 있습니다.

```Elixir 
value = 10
IO.inspect(value)
# 출력: 10
```
디버그 출력을 엔드포인트나 함수 체인 사이에 쉽게 삽입할 수 있습니다.

```Elixir 
value = 10
value |> IO.inspect |> Kernel.+(10)
# 출력: 10
# 결과: 20
```

## 깊이 있게 알아볼까요:

디버그 출력의 사용은 프로그래밍 초기부터 존재해왔고, 그 목적은 오류를 찾거나 개발자가 코드의 리얼타임 동작을 이해하는 데 도움을 주는 것입니다. 이밖에 디버그 출력에 대한 대안으로 로그 파일을 사용하거나 전문 디버깅 도구를 활용하는 방법이 있습니다. 

IO.inspect의 동작 방식은 단순합니다. 첫번째 인자로 받은 값을 그대로 반환하고, 그 값을 두번째 인자로 받은 디바이스(기본적으로 표준 출력)에 출력합니다.

## 참고자료:

1. Elixir 공식 문서의 IO.inspect: https://hexdocs.pm/elixir/IO.html#inspect/2
2. Elixir에서 Debugging의 좋은 예: https://elixirschool.com/en/lessons/specifics/debugging/
3. Elixir에서 로깅 요령: https://timber.io/blog/elixir-logging-the-ultimate-guide/