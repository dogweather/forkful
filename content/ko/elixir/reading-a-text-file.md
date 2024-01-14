---
title:                "Elixir: 텍스트 파일 읽기"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

Elixir 프로그래밍 블로그 포스트: 텍스트 파일 읽기

## 왜

텍스트 파일 읽기는 프로그래밍에서 매우 중요한 작업입니다. 텍스트 파일을 읽는 것은 사용자 입력을 받거나 파일에서 데이터를 추출하는 등 다양한 작업에서 필수적입니다. Elixir에서는 이를 위해 간단하고 효율적인 방법을 제공합니다.

## 방법

우선, 텍스트 파일을 읽기 위해 `File` 모듈을 불러와야 합니다. 그 뒤 `File.read` 함수를 사용하여 파일을 읽습니다. 아래는 간단한 예시 코드입니다.

```Elixir
filename = "sample.txt"
{:ok, file} = File.read(filename)
IO.puts(file)
```

위 코드에서 `filename` 변수는 읽을 파일의 경로와 이름을 나타내고, `{:ok, file}`을 통해 파일을 성공적으로 읽었음을 판단합니다. 마지막으로 `IO.puts` 함수를 사용하여 파일 내용을 출력합니다.

텍스트 파일을 한 줄씩 읽고 싶다면 `File.stream!` 함수와 `Stream.each` 함수를 사용하면 됩니다. 아래는 한 줄씩 파일을 읽고 출력하는 예시 코드입니다.

```Elixir
filename = "sample.txt"
{:ok, file} = File.stream!(filename)
file
|> Stream.each(&IO.puts/1)
|> Stream.run
```

## 깊이 들어가기

텍스트 파일을 읽는 방법에 대해 더 자세히 알아보겠습니다. `{:ok, file}`을 통해 파일을 읽으면 파일이 아니라 파일의 내용을 담고 있는 `iodata` 타입이 반환됩니다. 이를 통해 파일에서 필요한 데이터를 추출할 수 있습니다.

또한 `{:error, reason}` 형태로 반환되는 경우도 있습니다. 이 경우 파일이 읽을 수 없는 상태이기 때문입니다. 따라서 `File.read!` 함수를 사용하면 더 효율적으로 파일 읽기를 할 수 있습니다. 이 함수는 파일을 읽을 수 없는 경우 `File.Error` 예외를 발생시킵니다.

## 이와 비슷한 주제

- [Elixir 공식 문서 - File 모듈](https://hexdocs.pm/elixir/File.html)
- [Elixir 공식 문서 - Stream 모듈](https://hexdocs.pm/elixir/Stream.html)
- [File 모듈을 사용한 텍스트 파일 쓰기](https://example.com) "

## 참고

- [Markdown 문법](https://daringfireball.net/projects/markdown/syntax)
- [Elixir로 프로그래밍 시작하기](https://elixirko.github.io/blog/getting-started-with-elixir/)