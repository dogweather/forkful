---
title:    "Elixir: 텍스트 파일 읽기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

 Elixir에서 텍스트 파일을 읽는 것은 중요한 프로그래밍 기술입니다. 텍스트 파일을 읽고 쓰는 능력은 다양한 프로그램에서 데이터를 처리하는 데 필요합니다. 또한, Elixir에서 텍스트 파일을 읽는 것은 문제 해결 능력을 향상시키는 데 매우 유용합니다.

## 어떻게

첫 번째로, Elixir 문법을 사용하여 파일을 열고 읽는 방법을 배우겠습니다.

 ```elixir
file = File.open("hello.txt")
contents = IO.read(file)
IO.puts contents
 ```

위의 코드는 "hello.txt" 파일을 열고 내용을 읽어서 터미널에 출력하는 간단한 예제입니다. 이제 파일을 열고 명시적으로 닫는 방법을 살펴보겠습니다.

 ```elixir
file = File.open("hello.txt")
contents = IO.read(file)
IO.puts contents
File.close(file)
 ```

위의 코드는 파일을 읽은 후에 명시적으로 파일을 닫아줍니다. 파일을 닫지 않으면 메모리 누수가 발생할 수 있으므로 꼭 파일을 닫아주는 것이 중요합니다.

## Deep Dive

텍스트 파일을 읽는 또 다른 방법은 `File.stream!/2` 함수를 사용하는 것입니다. 이 함수는 파일을 스트림으로 열어서 파일 내용을 한 줄씩 읽을 수 있게 해줍니다.

 ```elixir
stream = File.stream!("hello.txt")
Enum.each(stream, fn line -> IO.puts line end)
 ```

위의 코드는 "hello.txt" 파일을 열고 각 줄을 읽어서 터미널에 출력합니다. 이 방법은 대용량 파일을 처리할 때 유용합니다.

## See Also

- Elixir 문서: https://elixir-lang.org/getting-started/introduction.html
- 파일 처리에 대한 더 많은 정보: https://hexdocs.pm/elixir/File.html