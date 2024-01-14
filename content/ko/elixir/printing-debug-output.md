---
title:    "Elixir: 디버그 출력 프린팅"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# 왜 디버그 출력을 사용해야 할까요?

디버그 출력은 디버깅 프로세스에서 매우 유용합니다. 코드의 한 부분만 실행하여 해당 부분이 어떻게 동작하는지 살펴볼 수 있습니다. 또한 출력을 통해 코드에서 발생하는 오류를 식별하고 수정할 수 있습니다.

## 어떻게 디버그 출력을 사용할까요?

다음은 Elixir에서 디버그 출력을 사용하는 간단한 예시입니다. 

```Elixir
IO.puts("Hello, world!")
```

위의 코드를 실행하면 "Hello, world!"라는 메시지가 터미널에 출력됩니다. 이를 통해 해당 코드가 제대로 동작하는지 확인할 수 있습니다.

또한 Elixir는 `IO.inspect/2` 함수를 제공하여 디버그 출력을 더욱 쉽게 사용할 수 있도록 합니다.

```Elixir
IO.inspect([1, 2, 3], label: "my list")
```

위의 코드를 실행하면 `[1, 2, 3]`이라는 리스트와 함께 "my list: "라는 라벨이 출력됩니다. 이를 통해 우리는 해당 리스트가 정확히 어떤 값으로 이루어져 있는지 알 수 있습니다.

## 깊게 파헤쳐보기

디버그 출력은 코드에서 발생하는 문제점을 식별하고 해결하는 데 매우 중요합니다. 하지만 항상 잘못된 부분을 찾기만 하는 것이 아니라, 코드의 흐름을 이해하고, 변수의 상태를 추적하는 데에도 도움을 줄 수 있습니다.

또한 `Logger` 모듈을 사용하여 디버그 출력을 더욱 구조적으로 관리할 수 있습니다. 이 모듈을 사용하면 로그 레벨을 설정하여 원하는 수준만 출력하도록 할 수 있으며, 로그 파일에 저장하는 등 추가적인 기능도 제공합니다.

## 관련 자료

- [Elixir 공식 문서 (한국어 번역)](https://elixir-lang.org/ko/docs.html)
- [Elixir 디버깅 팁 (번역된 위키 문서)](https://www.costela.net/archives/609)
- [Elixir 디버깅: IO.inspect와 Logger 모듈 (번역된 블로그 포스트)](https://calcpark.com/debugging-in-elixir/)

# 더 알아보기

여러분이 작성한 코드에서 디버그 출력을 제대로 활용할 수 있도록 더 많은 자료를 찾아보세요. 이를 통해 코드의 문제를 더욱 쉽게 식별하고, 효율적으로 해결할 수 있을 것입니다.