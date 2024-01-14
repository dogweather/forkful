---
title:                "Elixir: 표준 오류에 쓰는 방법"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

표준 에러에 대한 쓰기에 참여하는 이유를 간단히 설명합니다. 

코드를 디버깅하거나 오류를 파악할 때, 표준 에러에 쓰는 것은 매우 중요합니다.

## 어떻게 하면 될까요?

다음은 Elixir에서 표준 에러에 쓰는 예제 코드와 샘플 출력입니다. 

먼저, `IO.write/2` 함수를 사용하여 표준 에러에 쓰는 방법을 살펴보겠습니다. 

```Elixir
IO.write(:stderr, "This is an error message.")

# 샘플 출력:
# This is an error message.
```

다음으로, `Kernel.>>/2` 연산자를 사용하여 표준 에러로 값을 보낼 수 있습니다. 

```Elixir
"Error" >> File.stream!("errors.txt")

# errors.txt 파일에 저장됨:
# Error
```

마지막으로, `Logger.error/3` 함수를 사용하여 로그를 표준 에러에 쓸 수 있습니다. 

```Elixir
Logger.error("This is an error message.", tags: [:error])

# 샘플 출력:
# 12:34:56.789 [error] This is an error message.
```

## 깊이 파고들기

표준 에러에 쓰기는 코드 디버깅에 필수적입니다. 따라서 Elixir에서는 여러 가지 방법을 제공하여 개발자들이 효율적으로 디버깅할 수 있도록 도와줍니다. 

먼저, `IO.write/2` 함수를 사용하여 표준 에러에 쓴 메시지는 커널 모듈에 의해 자동으로 출력됩니다. 이는 로그 메시지보다 디버깅에 더 유용합니다. 

또한, `Kernel.>>/2` 연산자를 사용하여 파일에 값을 쓰는 것은 디버깅 용도로 사용할 때 유용한 방법입니다. 파일에 오류 메시지를 쓰면 로그 파일만으로 디버깅을 할 수 있으며, 코드를 재실행할 필요도 없습니다. 

마지막으로, `Logger.error/3` 함수를 사용하여 로그를 표준 에러에 쓰는 것은 로그나 파일에 쓰는 것보다 더 유용합니다. 이 함수는 로그 레벨을 지정할 수 있기 때문입니다. 따라서 오류가 발생한 위치나 상황에 따라 로그 레벨을 다르게 설정하여, 디버깅을 더 쉽게 할 수 있습니다.

## 참고 자료

- [Elixir 공식 문서 - IO 모듈](https://hexdocs.pm/elixir/master/IO.html)
- [Elixir 공식 문서 - File 모듈](https://hexdocs.pm/elixir/master/File.html)
- [Elixir 공식 문서 - Kernel 모듈](https://hexdocs.pm/elixir/master/Kernel.html)
- [Elixir 공식 문서 - Logger 모듈](https://hexdocs.pm/logger/Logger.html)