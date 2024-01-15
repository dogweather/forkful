---
title:                "csv 작업하기"
html_title:           "Elixir: csv 작업하기"
simple_title:         "csv 작업하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜

이 글을 쓰는 이유는 바로 CSV 파일과 Elixir의 조합에 대해 소개하기 위해서입니다. CSV 파일은 데이터를 저장하고 전달하기 위한 가장 일반적인 형식 중 하나이며, Elixir는 강력한 함수형 프로그래밍 언어이기 때문에 두 개를 결합하여 데이터를 처리하는 것은 효율적이고 재미있는 경험이 될 것입니다.

## 어떻게

CSV 파일과 Elixir를 함께 사용하려면, 먼저 파일을 열어서 데이터를 읽은 다음 Elixir에서 조작하고 다시 파일로 저장할 수 있습니다. 아래 코드는 Elixir에서 CSV 파일을 읽고, 열을 제거하고, 새로운 파일에 저장하는 간단한 예시입니다.

```Elixir
defmodule CSVManipulator do
  # CSV 파일을 읽어서 열 제거하기
  def remove_column(filename, column_name) do
    # 파일 열기
    file = File.open!(filename, [:read, :write])

    # 파일의 모든 라인을 읽기
    contents = IO.readlines(file)

    # 첫 번째 라인에서 열 이름 찾기
    headers = String.split(Enum.at(contents, 0), ",")

    # 제거할 열의 인덱스 찾기
    index = Enum.find_index(headers, &(&1 == column_name))

    # 각 줄에서 기존 열 제거하기
    modified_contents = Enum.map(contents, fn line ->
      line_columns = String.split(line, ",")
      Enum.delete_at(line_columns, index)
      |> Enum.join(",")
    end)

    # 새로운 파일에 쓰기
    File.write!(filename <> "_edited.csv", modified_contents)

    # 파일 닫기
    File.close(file)
  end
end
```

위 코드를 실행하면 파일에서 제공한 열을 제거하고, 제거된 열이 없는 새로운 파일이 생성됩니다.

## 깊게 파고들기

CSV 파일을 읽고 다시 저장하는 간단한 예시를 보여드렸지만, 실제로는 더 복잡한 작업을 할 수 있습니다. 예를 들어서, CSV 파일에서 특정 데이터를 추출하거나 열의 순서를 변경하거나 데이터를 정렬할 수 있습니다. 이 모든 작업들은 Elixir에서 손쉽게 할 수 있기 때문에, CSV 파일을 처리하는 데 있어 더욱 많은 자유도를 갖게됩니다.

## 또 다른 자료

- Elixir에서 파일 다루는 방법에 대한 자세한 설명: [https://elixir-lang.org/getting-started/file-operations.html](https://elixir-lang.org/getting-started/file-operations.html)
- CSV 파일 다루기에 대한 더 많은 예시와 코드: [https://hexdocs.pm/csv/CSV.html](https://hexdocs.pm/csv/CSV.html)
- Elixir의 다양한 함수형 프로그래밍 기능과 사용법: [https://elixir-lang.org/getting-started/modules-and-functions.html](https://elixir-lang.org/getting-started/modules-and-functions.html)