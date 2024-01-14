---
title:    "Elixir: 텍스트 파일 읽기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜?

일반적으로 텍스트 파일은 다양한 유형의 데이터를 저장하는 데 사용됩니다. Elixir 프로그램에서 텍스트 파일을 읽을 수 있다면, 여러분의 프로그램에서 상호작용할 수 있는 데이터가 확장될 수 있습니다. 이러한 이유로, 텍스트 파일을 읽는 방법을 배우는 것은 매우 유용합니다.

## 어떻게

먼저, 텍스트 파일을 읽기 위해서는 `File.open` 함수를 사용해야 합니다. 이 함수는 두 개의 인수를 취합니다. 첫 번째 인자는 파일의 경로를, 두 번째 인자는 파일을 열기 위해 사용할 옵션을 지정하는 리스트를 입력해야 합니다.

다음 코드는 "example.txt" 파일을 읽는 예시입니다.

```Elixir
# 파일 열기
file = File.open("example.txt", [:read])

# 파일 읽기
contents = IO.read(file)

# 파일 닫기
:ok = File.close(file)

# 파일의 내용 출력
IO.puts(contents)
```

위 코드에서 `IO.read` 함수를 사용하여 파일의 내용을 읽었고, `File.close` 함수를 사용하여 파일을 닫았습니다. 열어둔 파일을 잊지 않고 항상 닫는 것이 중요합니다.

## 더 깊게

텍스트 파일을 읽는 방법에는 여러 가지가 있습니다. `File.read` 함수를 사용하여 파일 전체를 한 번에 읽거나, `IO.stream` 함수를 사용하여 파일을 스트림 형태로 읽어 사용하는 것 등이 있습니다. 텍스트 파일을 다루는 더 많은 방법을 배우는 것은 여러분의 프로그래밍 스킬을 더욱 발전시킬 수 있을 것입니다.

## 더 알아보기

- [Elixir File.read 문서](https://hexdocs.pm/elixir/File.html#read/1)
- [Elixir IO.stream 문서](https://hexdocs.pm/elixir/IO.Stream.html)
- [Elixir 파일 처리 예시](https://elixirschool.com/ko/lessons/basics/basics/file-processing/)