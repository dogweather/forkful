---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

텍스트 파일 읽기는 프로그램이 디스크에 저장된 텍스트를 분석하고 사용하기 위해 텍스트 파일의 콘텐츠를 불러오는 과정입니다. 이는 설정 데이터, 로그 등을 읽어들이거나 텍스트 기반의 정보를 처리할 때 필요합니다.

## 어떻게 하나요:

```Elixir
{:ok, contents} = File.read("your_file.txt")
IO.puts(contents)
```

위의 예제에서, `File.read` 는 텍스트 파일의 내용을 가져오며, `IO.puts`는 가져온 내용을 출력합니다.

```Elixir
defmodule Read do
  def file do
    "your_file.txt"
    |> File.read()
    |> case do
        {:ok, contents} -> IO.puts(contents)
        {:error, reason} -> IO.inspect(reason)
       end
  end
end

Read.file
```

이 Elixir 모듈은 파일을 읽기 위한 더욱 안전한 방법을 보여줍니다. 가져오기를 시도하고, 성공하면 내용을 출력하고 실패하면 실패 이유를 출력합니다.

## 깊게 들어가기:

텍스트 파일 읽기는 컴퓨터 프로그래밍의 기본 중의 기본입니다. 그 이유는 프로그램이 외부 데이터와 상호 작용하는 가장 보편적인 방법 중 하나이기 때문입니다. Elixir에서는 `File` 모듈을 사용하여 이 작업을 수행합니다.

다른 방법으로는 `File.stream!` 함수를 사용하여 대용량 파일을 읽는 것도 가능합니다. 이 방법은 메모리 사용량을 줄이며 하나씩 줄을 처리할 수 있게 합니다.

```Elixir
File.stream!("your_large_file.txt")
|> Enum.each(&IO.puts/1)
```

내부적으로, Elixir의 `File.read` 및 `File.stream!` 함수는 Erlang의 더 낮은 수준의 함수를 기반으로 구현되어 있습니다. 이는 자체적으로 운영 체제의 기본 파일 핸들링 API를 사용합니다.

## 참고자료:

- [Elixir 공식 문서 - File module](https://hexdocs.pm/elixir/File.html)
- [Elixir 공식 문서 - IO module](https://hexdocs.pm/elixir/IO.html)
- [유용한 Elixir 파일 입출력 예제](https://www.tutorialspoint.com/elixir/elixir_file_io.htm)