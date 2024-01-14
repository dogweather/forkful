---
title:    "Elixir: 표준 오류에 쓰는 것"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜

일반적으로 프로그래밍을 하면서 프로그램의 실행 상태를 확인하기 위해 출력을 사용하지만, 때로는 오류 메시지나 중요한 정보를 스트림으로 보내야할 때가 있습니다. 이때 "표준 에러" 스트림에 코드를 작성하여 오류를 디버깅하거나 데이터를 기록할 수 있습니다. 이번 포스트에서는 Elixir에서 표준 에러를 활용하는 방법에 대해 알아보겠습니다.

## 방법

먼저, 표준 에러 스트림으로 데이터를 기록하기 위해서는 `Logger` 모듈을 사용해야 합니다. `Logger.error/1` 함수를 사용하여 메시지를 표준 에러로 보낼 수 있습니다. 아래의 예시 코드를 살펴보겠습니다.

```Elixir
Logger.error("This is an error message")
```

위의 코드를 실행하면, 다음과 같은 출력이 나타납니다.

```Elixir
15:35:52.123 [error] This is an error message
```

또한, 표준 에러에 데이터를 기록할 때, `Logger.error/4` 함수를 사용하여 더욱 특정한 형식으로 메시지를 작성할 수 있습니다. 아래의 예시 코드를 살펴보겠습니다.

```Elixir
Logger.error("An error occurred in process ~p. Error code: ~p", [pid, error_code], application: :my_app)
```

위의 코드를 실행하면, 다음과 같은 형식으로 메시지가 출력됩니다.

```Elixir
15:35:52.123 [error] An error occurred in process #PID<0.1234.0>. Error code: 500 [application: :my_app]
```

## 깊게 살펴보기

Elixir의 `Logger` 모듈은 여러가지 유용한 기능들을 제공합니다. 우리는 `Logger.error/4` 외에도, `Logger.warn/4`, `Logger.debug/4`, `Logger.info/4`와 같은 다른 함수들을 사용하여 다양한 레벨의 정보를 표준 에러로 보낼 수 있습니다. 또한, `Logger` 모듈에서 제공하는 설정 옵션들을 통해 출력 형식을 변경하거나 로그 파일에 저장하는 등의 방법으로 로깅을 더욱 다양하게 사용할 수 있습니다.

## 관련 자료

Elixir의 `Logger` 모듈에 대해 더 자세히 알아보고 싶다면 아래의 링크들을 참고해보세요:

- [Elixir 공식 문서 - Logger 모듈](https://hexdocs.pm/elixir/Logger.html)
- [Learn Elixir in Y minutes - Logging](https://learnxinyminutes.com/docs/ko-kr/elixir-kr/#logging)