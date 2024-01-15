---
title:                "표준 에러에 쓰는 것"
html_title:           "Gleam: 표준 에러에 쓰는 것"
simple_title:         "표준 에러에 쓰는 것"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

이 글은 데브로스(Dave Thomas)의 [Gleam](https://gleam.run/) 프로그래밍 언어에 대한 간단한 소개입니다. 오늘의 주제는 글을 표준 오류(standard error)에 쓰는 방법입니다. 프로그래머라면 한번쯤은 글을 표준 오류에 쓰는 일이 있을 것입니다. 그렇다면 그 이유는 무엇일까요?

하나는 디버깅(debugging)이 있을 수 있습니다. 프로그램이 실행되는 동안 에러 메시지를 표준 오류에 출력하면, 프로그래머는 콘솔(console)에서 에러 메시지를 확인할 수 있어 디버깅을 더 쉽게 할 수 있습니다. 또 다른 이유는 로깅(logging)입니다. 일반적으로 이 로그는 텍스트 파일로 남지만, 표준 오류에 쓴다면 콘솔에서 바로 확인할 수 있습니다. 이를 통해 프로그램의 실행 상태를 실시간으로 모니터링할 수 있습니다.

## 어떻게

글을 표준 오류에 쓰는 것은 매우 간단합니다. 다음과 같은 코드를 추가하면 됩니다:

```Gleam
error_msg := "이것은 에러 메시지입니다!"
io.stderr(error_msg)
```

위 코드를 실행하면 `이것은 에러 메시지입니다!`라는 문자열이 콘솔에 출력됩니다. 굉장히 간단하죠? 화장표(faceplate)에도 같은 방식으로 쓸 수 있습니다. 다음과 같이 `write_error` 함수를 정의할 수 있습니다:

```Gleam
pub fn write_error(error: String) {
  io.stderr("에러: " ++ error)
}
```

위 함수를 사용하면 에러 메시지를 더 이쁘게 출력할 수 있습니다. 예를 들어 화장표를 사용하면 다음과 같이 출력됩니다:

```Gleam
write_error("이것은 에러 메시지입니다!")
```

아래와 같은 표준 오류 메시지가 출력됩니다:

```
에러: 이것은 에러 메시지입니다!
```

## 더 깊게

자, 이제 여러분은 표준 오류에 글을 쓰는데 필요한 기초들을 알게 되었습니다. 그러나 상황에 따라서는 이보다 더 복잡한 처리를 원할 수도 있습니다. 이를 위해 `erlang:stderr()` 함수를 사용할 수 있습니다. 이 함수를 사용하면 표준 오류를 다른 프로세스에서도 다룰 수 있습니다. Gleam에서 다른 프로세스를 생성하는 방법은 `spawn` 함수를 사용하는 것입니다. 이 함수를 이용하면 아래와 같이 코드를 작성할 수 있습니다:

```Gleam
pid := erlang:spawn(fn()-> io.fputs(erlang:stderr(), "이것은 다른 프로세스에서 작동하는 에러 메시지입니다!") end)
```

위 코드는 다른 프로세스에서 표준 오류에 에러 메시지를 출력하게 됩니다. 이렇게 하면 메인 프로세스에서 다른 작업을 하면서도 동시에 표준 오류를 다룰 수 있습니다.

## 참고자료

- [Gleam 문서](https://gleam.run/getting-started/installation.html)
- [Erlang 문서](https://erlang.org/doc/index.html)
- [Gleam 커뮤니티 포럼](https://forum.gleam.run/)