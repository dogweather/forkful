---
title:                "텍스트 파일 읽기"
date:                  2024-01-20T17:54:12.651554-07:00
model:                 gpt-4-1106-preview
simple_title:         "텍스트 파일 읽기"

category:             "Elixir"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 읽기는 파일에서 데이터를 추출하는 것입니다. 프로그래머는 설정, 사용자 데이터, 혹은 로그 분석 등을 위해 이를 수행합니다.

## How to: (방법)
Elixir에서 텍스트 파일 읽기는 간단합니다. `File.read/1` 함수 사용 예제를 보시죠:

```elixir
# 파일 읽기
{:ok, content} = File.read("hello.txt")

# 내용 출력
IO.puts(content)
```

만약 `hello.txt` 파일에 "안녕, 세상!" 이라고 쓰여 있다면, 출력 예제는 다음과 같습니다:

```
안녕, 세상!
```

## Deep Dive (심층 분석)
Elixir에서 파일 읽기는 Erlang VM에서 처리됩니다. 직접 파일 시스템을 다루는 건 보통 더 낮은 수준의 언어에서만 가능합니다. Elixir는 Erlang의 기능을 내장하고 있어 이런 저수준의 작업도 가능하죠. 

스트림을 사용해 대용량 파일을 처리할 수도 있습니다. 예를 들어, `File.stream!/3` 사용:

```elixir
File.stream!("large_file.txt")
|> Enum.each(fn line -> IO.puts(line) end)
```

이 방식은 메모리 사용량을 줄이고, 파일의 각 줄을 순차적으로 처리합니다. 

`File.read/1`과 `File.stream!/3`은 기본 제공되는 두 가지 옵션입니다만, 추가적으로 `:file` 모듈을 통해 더 많은 기능을 이용할 수도 있습니다.

## See Also (관련 자료)
- [Elixir's File module documentation](https://hexdocs.pm/elixir/File.html)
- [Programming Elixir ≥ 1.6 book by Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
