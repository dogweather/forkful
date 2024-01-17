---
title:                "디버그 출력 프린팅"
html_title:           "Elixir: 디버그 출력 프린팅"
simple_title:         "디버그 출력 프린팅"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
디버그 출력을 프로그래머들이 하는 이유는 코드의 실행 중 어떤 값이 어떻게 변경되고 있는지 확인할 수 있기 때문입니다. 이는 버그를 찾는 데 도움이 되며, 코드를 디버깅하고 수정하는 데 도움이 됩니다.

## 방법:
디버그 출력을 하기 위해선, 코드 내에서 **IO.inspect** 함수를 사용하면 됩니다. 다음은 코드에서 IO.inspect 함수를 이용해 디버그 출력을 하는 간단한 예시입니다:
```elixir
# 코드 예시
x = 42
IO.inspect(x)
```
**IO.inspect** 함수는 전달받은 값을 콘솔에 출력해줍니다. 위의 예시 코드를 실행하면, 다음과 같이 콘솔에 출력됩니다:
```elixir
# 출력 예시
42
```

## 깊게 파고들기:
디버그 출력의 역사적 배경은 아주 오래전부터 있는 것은 아니지만, 최근에 프로그래밍 언어와 개발 도구에서 더 많이 사용되고 있습니다. 다른 디버깅 방법으로는 디버거를 사용하는 것이 있지만, 이는 코드를 멈추고 변수의 값을 살펴보는 것이 아니기 때문에 디버그 출력은 여전히 유용합니다. 디버그 출력을 할 때 생각해야 할 점은, 실행 중인 코드에서도 이를 사용하면 성능에 영향을 줄 수 있다는 것입니다.

## 관련 자료:
- Elixir 공식 문서: https://elixir-lang.org/getting-started/debugging.html#inspect
- Elixir forum의 관련 질문과 답변: https://elixirforum.com/t/help-understanding-ex-output/8553