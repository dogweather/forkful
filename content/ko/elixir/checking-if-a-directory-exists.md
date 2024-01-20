---
title:                "디렉토리의 존재 여부 확인하기"
html_title:           "Arduino: 디렉토리의 존재 여부 확인하기"
simple_title:         "디렉토리의 존재 여부 확인하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

디렉토리 존재 여부 확인은 파일 시스템에 특정 디렉토리가 존재하는지 알아보는 것입니다. 이를 통해 스크립트나 애플리케이션에서 파일 작업을 수행하기 전에 에러를 예방할 수 있습니다.

## How to: (어떻게 하나요?)

Elixir에서는 `File` 모듈의 `dir?/1` 함수를 사용해 디렉토리의 존재 여부를 쉽게 확인할 수 있습니다. 예제 코드와 결과를 보겠습니다:

```elixir
# 디렉토리의 존재 여부 확인
is_directory = File.dir?("/path/to/directory")

# 결과 출력
IO.puts(is_directory)
```

이 코드가 실행되면, 해당 경로에 디렉토리가 있다면 `true`, 아니면 `false`를 출력할 것입니다.

## Deep Dive (심화 탐구)

역사적으로 파일 시스템에 접근하는 방법은 운영 체제마다 다릅니다. Elixir는 Erlang VM (BEAM) 위에서 동작하기 때문에, BEAM이 제공하는 파일 시스템에 대한 추상화를 활용합니다. 이것이 `File.dir?/1` 함수가 모든 주요 운영 체제에서 일관된 결과를 제공할 수 있는 이유입니다.

대안으로는 `:filelib.is_dir/1`을 사용할 수도 있지만, 일반적으로 `File` 모듈을 사용하는 것이 Elixir에서의 관례입니다.

구현 세부사항으로, `File.dir?/1` 함수는 내부적으로 파일 시스템의 메타데이터에 접근하여 디렉토리의 속성을 확인합니다. 이는 파일 시스템에 따라 완전한 검사일 수도 있고, 단순히 파일 매트릭스 정보를 확인하는 것일 수도 있습니다.

## See Also (관련 자료)

- Elixir `File` 모듈 공식 문서: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Erlang `:filelib` 모듈의 설명: [http://erlang.org/doc/man/filelib.html](http://erlang.org/doc/man/filelib.html)
- Elixir School (한국어): 기초부터 시작하는 Elixir 학습 사이트 [https://elixirschool.com/ko](https://elixirschool.com/ko)