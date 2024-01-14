---
title:                "Elixir: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것은 프로그래밍에서 매우 중요합니다. 특히 Elixir에서는 파일 시스템과 상호 작용하는 데 필수적입니다.

## 어떻게

먼저 `{Path, basename}` 모듈을 사용하여 파일 경로를 정의해야합니다. 그런 다음 `File.stat()` 함수를 사용하여 디렉토리가 있는지 확인할 수 있습니다. 아래는 예제 코드입니다:

```elixir
path = Path.expand("example/directory")
File.stat(path)
```

위 코드는 디렉토리가 존재하지 않는 경우 `{:error, :enoent}`과 같은 에러를 반환합니다. 디렉토리가 존재하는 경우 `{:ok, dirent}`와 같은 경로가 반환됩니다.

## 깊이 파고들기

더 나은 방법으로는 Erlang의 `:filelib` 모듈을 사용하는 것입니다. 이 모듈은 `filelib:wildcard()` 함수를 통해 확장 된 유닉스 셸 패턴을 지원합니다. 예를 들어 `filelib:wildcard("example/*.txt")`는 `{:ok, ["example/file1.txt", "example/file2.txt"]}`와 같은 결과를 반환합니다.

## See Also

- [Elixir String 모듈 문서](https://hexdocs.pm/elixir/String.html)
- [깊이 파고들기](https://elixirschool.com/lessons/basics/io/#file-operations)
- [Erlang File 모듈 문서](http://erlang.org/doc/man/file.html#dir-1)