---
title:                "Elixir: 컴퓨터 프로그래밍의 '명령줄 인수 읽기'"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 것에 대해 이야기하고 있는 이유는 무엇일까요? 프로그래밍을 하는 사람들이라면 누구나 커맨드 라인 프로그램을 작성할 때 손쉽게 인수를 읽을 수 있도록 알아야 합니다. 이를 통해 더 효율적이고 유연한 코드를 작성할 수 있기 때문입니다.

## 해보는 법

다음은 Elixir에서 커맨드 라인 인수를 읽는 방법을 보여주는 예제 코드입니다.

```
Elixir 정규식으로 문자열 분할
```

위 코드에서는 `OptionParser` 모듈을 사용하여 커맨드 라인 인수를 읽고, `Regex` 모듈을 사용하여 문자열을 분할합니다. 이를 통해 간단하고 쉽게 인수를 분석할 수 있습니다.

아래는 위 코드를 실행한 결과입니다.

```
관리자$ elixir args.exs -f input.txt -n 10 -v

[
  file_name: "input.txt",
  num_lines: 10,
  verbose: true
]
```

위 결과에서 볼 수 있듯이, 인수를 손쉽게 읽고 해당하는 값을 받아올 수 있습니다.

## 깊이 파헤치기

`OptionParser` 모듈을 사용하면 더 많은 기능을 활용할 수 있습니다. 예를 들어, 기본값을 설정할 수 있고, 각 인수에 대한 설명을 추가할 수도 있습니다.

또한 `Regex` 모듈을 사용하지 않고도 `OptionParser` 모듈만으로 인수를 분석할 수 있습니다. 이를 통해 더 간단하고 효율적인 코드를 작성할 수 있습니다.

## 관련 자료

- [Elixir 공식 문서](https://hexdocs.pm/elixir/OptionParser.html)
- [Awesome Elixir Github 저장소](https://github.com/h4cc/awesome-elixir#command-line-applications)