---
title:                "Elixir: 디버그 출력하기"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜 

디버그 출력을 사용해야 하는 이유는 단순합니다. 문제를 해결하는 과정에서 코드의 흐름을 파악하고, 예상치 못한 버그를 발견하는 데 도움이 됩니다. 디버깅은 모든 프로그래머에게 중요한 기술이며, 디버그 출력은 이를 더욱 쉽게 만들어줍니다.

## 하는 법

디버그 출력은 `IO.inspect/2` 함수를 사용하여 쉽게 할 수 있습니다. 이 함수는 인자로 받은 데이터를 출력하고, 해당 값을 그대로 반환합니다. 아래 예시를 참고해 보세요.

```elixir
IO.inspect(10)
```
출력: 
```
10
10
```

함수의 결과가 `10`이고, 함수 자체의 반환 값도 `10`인 것을 볼 수 있습니다. 기본값으로는 터미널에 출력되지만, `IO.inspect/2` 함수에 옵션 값을 설정하여 파일 등 다른 장소에 출력할 수도 있습니다.

## 깊이 파고들기

디버그 출력은 여러 가지 옵션 값을 설정하여 더욱 유용하게 사용할 수 있습니다. 예를 들어, `label` 옵션을 사용하면 출력 값의 라벨을 지정할 수 있습니다. `color` 옵션을 사용하면 출력에 색상을 적용하여 눈으로 구분하기 쉽게 만들 수 있습니다. 자세한 내용은 공식 문서를 참고해 보세요.

## 참고

- [Elixir 공식 문서 - 디버그 출력](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Elixir Koans - 디버그 출력](http://elixirkoans.io/debugging/2)