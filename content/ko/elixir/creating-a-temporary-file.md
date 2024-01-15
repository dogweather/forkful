---
title:                "임시 파일 생성하기"
html_title:           "Elixir: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 생성하는 일에 참여하는 이유는 여러 가지가 있을 수 있습니다. 한 가지로는 임시 데이터를 저장하기 위해서이지만, 더 자세하게 살펴보면 프로그래밍에서 임시 파일은 중간 과정에서 매우 유용하게 사용될 수 있습니다.

## 어떻게

먼저, `File.tempfile/2` 함수를 사용하여 Elixir에서 임시 파일을 생성할 수 있습니다. 인자로는 파일에 쓰일 내용과 파일 이름의 접두어를 전달해야 합니다. 다음은 간단한 예제 코드입니다.

```Elixir
{path, file} = File.tempfile("prefix_")
IO.write(path, "Hello, world!")
```

위 코드에서 `path` 변수는 생성된 임시 파일의 경로를, `file` 변수는 해당 파일의 이름을 나타냅니다. 이후에는 실행되는 환경에 따라 파일을 읽고 삭제하는 등의 작업을 추가할 수 있습니다.

## 딥 다이브

임시 파일은 프로그래밍에서 매우 중요한 역할을 할 수 있습니다. 예를 들어, 특정 작업을 수행하기 위해 다소 복잡한 데이터를 처리해야 한다고 가정해 봅시다. 이러한 경우에는 임시 파일을 생성하여 그 과정에서 발생하는 데이터를 임시로 저장할 수 있습니다. 또한 테스트 코드 작성 시에도 임시 파일을 사용하여 데이터를 모의(mock)할 수 있습니다.

## 참고 문서

- [Elixir 문서: File.tempfile/2](https://hexdocs.pm/elixir/File.html#tempfile/2)
- [ProgrammingElixir: Creating Temporary Files](https://programmingelixir.com/creating-temporary-files/)