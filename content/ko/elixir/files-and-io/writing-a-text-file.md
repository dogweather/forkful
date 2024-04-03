---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:52.285950-07:00
description: "\uBC29\uBC95: Elixir\uB294 \uB0B4\uC7A5 \uBAA8\uB4C8\uB85C \uD30C\uC77C\
  \ \uD578\uB4E4\uB9C1\uC744 \uAC04\uB2E8\uD558\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4. \uD30C\
  \uC77C\uC5D0 \uC4F0\uB294 \uC8FC\uC694 \uBC29\uBC95\uC740 `File.write/2` \uB610\uB294\
  \ `File.write!/2` \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774\uBA70\
  , \uC804\uC790\uB294 `:ok` \uB610\uB294 `:error` \uD29C\uD50C\uC744 \uBC18\uD658\
  \uD558\uACE0 \uD6C4\uC790\uB294 \uC2E4\uD328 \uC2DC \uC624\uB958\uB97C \uBC1C\uC0DD\
  \uC2DC\uD0B5\uB2C8\uB2E4.\u2026"
lastmod: '2024-03-13T22:44:54.748588-06:00'
model: gpt-4-0125-preview
summary: "Elixir\uB294 \uB0B4\uC7A5 \uBAA8\uB4C8\uB85C \uD30C\uC77C \uD578\uB4E4\uB9C1\
  \uC744 \uAC04\uB2E8\uD558\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
Elixir는 내장 모듈로 파일 핸들링을 간단하게 만듭니다. 파일에 쓰는 주요 방법은 `File.write/2` 또는 `File.write!/2` 함수를 사용하는 것이며, 전자는 `:ok` 또는 `:error` 튜플을 반환하고 후자는 실패 시 오류를 발생시킵니다.

간단한 예시는 다음과 같습니다:

```elixir
# 파일에 쓰기, 간단한 메시지
File.write("hello.txt", "Hello, World!")

# 코드를 실행하면 'hello.txt'가 "Hello, World!" 내용으로 생성됩니다
```

파일에 추가하기 위해서는 `File.open/3`을 `[:write, :append]` 옵션과 함께 사용하고 그 다음에 `IO.binwrite/2`로 내용을 추가합니다:

```elixir
# 파일에 추가하기
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nLet's add another line.")
File.close(file)

# 이제 'hello.txt'는 두 번째 줄 "Let's add another line."을 포함합니다.
```

큰 데이터를 다루거나 쓰기 과정을 더 많이 제어할 필요가 있는 경우, 파일에 데이터를 지연 쓰기(lazily)하기 위해 `Stream` 모듈을 사용할 수 있습니다:

```elixir
# 큰 데이터셋을 지연 쓰기하기
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Number: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# 이것은 'numbers.txt'를 생성하며, 각각 새로운 줄에 0부터 9까지의 숫자를 씁니다.
```

보다 정교한 파일 핸들링이 필요한 프로젝트의 경우, CSV 파일 조작을 위한 맞춤 기능을 제공하는 `CSV`와 같은 제3자 라이브러리를 살펴볼 수 있지만, 많은 목적에 대해, Elixir의 내장 기능만으로도 충분하다는 것을 기억하세요.
