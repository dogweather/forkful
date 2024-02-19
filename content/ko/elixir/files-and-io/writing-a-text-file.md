---
aliases:
- /ko/elixir/writing-a-text-file/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:52.285950-07:00
description: "Elixir\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC5D0 \uC4F0\uAE30\
  \uB294 \uAC1C\uBC1C\uC790\uC5D0\uAC8C \uD544\uC218\uC801\uC778 \uAE30\uC220\uB85C\
  , \uB370\uC774\uD130 \uC601\uC18D\uC131, \uB85C\uAE45 \uB610\uB294 \uC778\uAC04\uC774\
  \ \uC77D\uC744 \uC218 \uC788\uB294 \uB0B4\uC6A9\uC744 \uB0B4\uBCF4\uB0B4\uB294 \uB370\
  \uC5D0 \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC774\uB97C \uD1B5\uD574 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uC0C1\uD0DC, \uB514\
  \uBC84\uADF8 \uC815\uBCF4, \uAD6C\uC131 \uB610\uB294 \uD14D\uC2A4\uD2B8\uC640 \uAC19\
  \uC740 \uBCF4\uD3B8\uC801 \uD615\uC2DD\uC744 \uC120\uD638\uD558\uB294 \uC2DC\uC2A4\
  \uD15C \uAC04 \uB370\uC774\uD130\u2026"
lastmod: 2024-02-18 23:09:05.783786
model: gpt-4-0125-preview
summary: "Elixir\uC5D0\uC11C \uD14D\uC2A4\uD2B8 \uD30C\uC77C\uC5D0 \uC4F0\uAE30\uB294\
  \ \uAC1C\uBC1C\uC790\uC5D0\uAC8C \uD544\uC218\uC801\uC778 \uAE30\uC220\uB85C, \uB370\
  \uC774\uD130 \uC601\uC18D\uC131, \uB85C\uAE45 \uB610\uB294 \uC778\uAC04\uC774 \uC77D\
  \uC744 \uC218 \uC788\uB294 \uB0B4\uC6A9\uC744 \uB0B4\uBCF4\uB0B4\uB294 \uB370\uC5D0\
  \ \uC0AC\uC6A9\uB429\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\
  \uB97C \uD1B5\uD574 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uC0C1\uD0DC, \uB514\uBC84\
  \uADF8 \uC815\uBCF4, \uAD6C\uC131 \uB610\uB294 \uD14D\uC2A4\uD2B8\uC640 \uAC19\uC740\
  \ \uBCF4\uD3B8\uC801 \uD615\uC2DD\uC744 \uC120\uD638\uD558\uB294 \uC2DC\uC2A4\uD15C\
  \ \uAC04 \uB370\uC774\uD130\u2026"
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
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
