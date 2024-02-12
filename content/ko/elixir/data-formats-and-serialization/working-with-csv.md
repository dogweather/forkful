---
title:                "CSV와 함께 작업하기"
aliases:
- /ko/elixir/working-with-csv/
date:                  2024-02-03T19:19:43.484290-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV (쉼표로 구분된 값) 파일을 작업하는 것은 이러한 파일에서 데이터를 읽고 쓰는 것을 포함하며, 데이터 가져오기/내보내기 또는 간단한 저장 솔루션을 필요로 하는 작업에 대한 일반적인 필요성입니다. 프로그래머들은 시스템 간의 데이터 교환, 빠른 데이터 편집, 또는 가볍고 쉽게 조작할 수 있는 데이터 형식이 유리한 상황에서 이 기능을 활용합니다.

## 방법:

강력한 패턴 매칭과 파이프라이닝 지원을 갖춘 Elixir는 외부 라이브러리 없이도 효율적으로 CSV 파일을 처리할 수 있습니다. 그러나 보다 고급 필요성에 대해서는 `nimble_csv` 라이브러리가 빠르고 간단한 선택입니다.

### 외부 라이브러리 없이 CSV 파일 읽기

Elixir의 내장 함수를 사용하여 CSV 파일을 읽고 파싱할 수 있습니다:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# 예제 사용
CSVReader.read_file("data.csv")
# 출력: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### CSV 파일에 데이터 쓰기

마찬가지로, CSV 파일에 데이터를 쓰려면:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# 예제 사용
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# output.csv 파일을 생성하고 CSV 형식으로 데이터를 포맷합니다
```

### `nimble_csv` 사용하기

더 복잡한 CSV 처리를 위해 `nimble_csv`는 CSV 데이터를 작업하는 강력하고 유연한 방법을 제공합니다. 우선, `mix.exs`에서 `nimble_csv`를 의존성에 추가하고 `mix deps.get`을 실행하세요:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

`nimble_csv`를 사용하여 CSV 데이터 파싱하기:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# 예제 사용
MyCSVParser.parse("data.csv")
# `nimble_csv`를 사용한 출력은 파서 설정에 따라 다르지만, 일반적으로 리스트의 리스트나 튜플 형태로 보입니다.
```

`nimble_csv`를 사용하여 CSV 데이터를 쓰는 것은 적절한 포맷으로 데이터를 수동으로 변환한 다음 파일에 쓰는 것을 요구하며, 이는 평범한 Elixir 예제처럼 진행되지만, 정확하게 포맷된 CSV 행을 생성하기 위해 `nimble_csv`를 활용합니다.

작업의 복잡성에 적합한 접근 방식을 선택함으로써, Elixir에서 CSV 파일을 유연하고 강력하게 다룰 수 있습니다.
