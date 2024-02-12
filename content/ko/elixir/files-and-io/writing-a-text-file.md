---
title:                "텍스트 파일 쓰기"
aliases: - /ko/elixir/writing-a-text-file.md
date:                  2024-02-03T19:27:52.285950-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Elixir에서 텍스트 파일에 쓰기는 개발자에게 필수적인 기술로, 데이터 영속성, 로깅 또는 인간이 읽을 수 있는 내용을 내보내는 데에 사용됩니다. 프로그래머들은 이를 통해 애플리케이션 상태, 디버그 정보, 구성 또는 텍스트와 같은 보편적 형식을 선호하는 시스템 간 데이터 교환을 저장하게 됩니다.

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
