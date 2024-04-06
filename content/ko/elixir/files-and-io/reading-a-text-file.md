---
date: 2024-01-20 17:54:12.651554-07:00
description: "How to: (\uBC29\uBC95) Elixir\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\
  \ \uC77D\uAE30\uB294 \uAC04\uB2E8\uD569\uB2C8\uB2E4. `File.read/1` \uD568\uC218\
  \ \uC0AC\uC6A9 \uC608\uC81C\uB97C \uBCF4\uC2DC\uC8E0."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.576137-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Elixir\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\
  \uAE30\uB294 \uAC04\uB2E8\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
weight: 22
---

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
