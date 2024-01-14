---
title:                "Elixir: 텍스트 파일 작성하기"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 쓰는 것의 중요성은 매우 간단합니다. 프로그래밍에서 텍스트 파일은 데이터를 저장하고 읽는데 매우 유용한 방법입니다. 또한 이를 통해 프로그램의 사용자와 상호작용 할 수 있습니다.

## 사용 방법

```Elixir
defmodule FileExamples do
  # 텍스트 파일 생성
  def create_file do
    File.write("text_file.txt", "Hello, world!")
  end

  # 텍스트 파일 읽기
  def read_file do
    text = File.read("text_file.txt")
    IO.puts(text) # 출력: Hello, world!
  end
end
```

위의 예제에서는 File 모듈을 사용하여 텍스트 파일을 생성하고 읽는 방법을 보여줍니다. "File.write" 함수는 두 개의 인수를 받는데, 하나는 파일의 이름이고 다른 하나는 파일에 추가 될 내용입니다. 마찬가지로 "File.read" 함수는 파일 이름만 인수로 받습니다.

## 깊이 파헤치기

보다 깊이 안다면 우리는 "File.write" 함수가 새 파일을 생성하거나 이미 존재하는 파일을 덮어쓴다는 것을 알 수 있습니다. 또한 "File.read" 함수는 파일에서 읽은 내용을 문자열로 반환한다는 것을 알 수 있습니다.

## 참고 자료

- [File 모듈 문서](https://hexdocs.pm/elixir/File.html)
- [Elixir 파일 작성하기](https://elixir-lang.org/getting-started/file-operations.html#files-reading-and-writing)
- [Elixir 텍스트 파일 처리 예제](https://www.programming-elixir.com/categories/elixir-io/reading-and-writing-text-files/#_read_and_write_text_files)