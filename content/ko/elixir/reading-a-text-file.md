---
title:                "Elixir: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 대해 궁금하지 않으신가요? 이 블로그 포스트에서는 Elixir 프로그래밍 언어를 사용하여 텍스트 파일을 읽는 방법에 대해 알려드릴 것입니다.

## 어떻게

우선, Elixir에서 텍스트 파일을 읽기 위해서는 File 모듈을 사용해야 합니다. 다음은 File 모듈을 사용하여 텍스트 파일을 읽는 간단한 예제입니다.

```Elixir
# 파일 경로를 지정합니다.
file_path = "./text.txt"

# 파일을 엽니다.
file = File.open(file_path)

# 파일의 내용을 읽어옵니다.
contents = File.read(file)

# 파일을 닫습니다.
File.close(file)

# 읽어온 내용을 출력합니다.
IO.puts(contents)
```

위 코드를 실행하면 `text.txt` 파일의 내용이 출력될 것입니다.

## 깊이 파고들기

위 예제에서는 간단하게 파일을 열고 읽는 방법을 알려드렸습니다. 하지만 실제로는 파일을 읽을 때 유의해야 할 점들이 있습니다.

먼저, 파일을 열 때 `File.open/2` 함수의 두 번째 인자로 파일을 여는 모드를 지정할 수 있습니다. 예를 들어 "r"은 읽기 모드를 나타내고, "w"는 쓰기 모드를 나타냅니다. 또한 `File.close/1` 함수를 사용하지 않더라도 Elixir에서는 자동으로 파일을 닫아주는 기능이 있으니 안심하셔도 좋습니다.

또한, 파일을 읽을 때 한글 인코딩에 대해 주의해야 합니다. Elixir의 기본 인코딩은 UTF-8이므로 만약 파일의 인코딩이 다르다면 `File.read/2` 함수의 두 번째 인자로 인코딩 타입을 지정해 주어야 정상적으로 읽을 수 있습니다.

## 관련 글

- [Elixir File 모듈 공식 문서](https://hexdocs.pm/elixir/File.html)
- [Elixir의 자동 파일 닫기 기능](https://elixir-lang.org/getting-started/processes.html#automatic-closing-of-files)
- [Elixir의 인코딩 타입 지정 방법](https://hexdocs.pm/elixir/String.html#module-encode-decode-example-using-erlang-b ↗)