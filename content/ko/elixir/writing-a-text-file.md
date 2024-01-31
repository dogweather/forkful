---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
텍스트 파일 쓰기란, 데이터를 텍스트 형식으로 파일에 저장하는 것입니다. 프로그래머들은 데이터 저장, 로그 기록, 설정 관리 등을 위해 이를 수행합니다.

## How to:
Elixir에서 파일에 텍스트를 쓰려면 `File.write/2`나 `File.open/3` 후 `IO.write/2`를 사용하세요.

```elixir
# 간단하게 파일에 "Hello, World!" 쓰기
File.write("hello.txt", "Hello, World!")

# 결과 확인
File.read!("hello.txt")
# => "Hello, World!"

# 파일 열고 데이터 쓰기, 그리고 닫기
{:ok, file} = File.open("hello.txt", [:write])
IO.write(file, "안녕, 세계!")
File.close(file)

# 결과 확인
File.read!("hello.txt")
# => "안녕, 세계!"
```

## Deep Dive
텍스트 파일 쓰기는 초기 컴퓨팅부터 있어왔다. `File.write/2`는 편리하지만 큰 데이터에는 적합하지 않을 수 있다. 그럴 땐 `File.stream!/3`로 스트림을 다루거나, `:io.put_chars/2` 같은 Erlang 함수를 사용할 수 있다. Elixir는 내부적으로 Erlang의 파일 처리 기능을 활용한다.

## See Also
- Elixir 공식 문서: https://hexdocs.pm/elixir/File.html
- Erlang `:io` 모듈: http://erlang.org/doc/man/io.html
- Learn Elixir: https://elixir-lang.org/learning.html
