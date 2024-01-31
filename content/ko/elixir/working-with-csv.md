---
title:                "CSV 파일 다루기"
date:                  2024-01-19
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV(Comma-Separated Values)는 데이터를 저장하고 교환하는 인기 있는 형식입니다. 프로그래머는 테이블 데이터를 쉽게 교환하고 분석하기 위해 CSV를 사용합니다.

## How to:
CSV 파일을 읽고, 쓰고, 변환하는 방법을 알아봅시다.

```elixir
# CSV 파일 읽기
{:ok, data} = File.read("example.csv")
csv_data = String.split(data, "\n")

# CSV 파싱
parsed_csv = Enum.map(csv_data, &String.split(&1, ","))

# 결과 출력
IO.inspect parsed_csv

# CSV 파일 쓰기
content = [
  ["name", "age", "city"],
  ["alice", "30", "seoul"],
  ["bob", "22", "busan"]
]

File.write("new_example.csv", content
|> Enum.map(&Enum.join(&1, ","))
|> Enum.join("\n"))
```

Sample output:
```
[
  ["name", "age", "city"],
  ["alice", "30", "seoul"],
  ["bob", "22", "busan"]
]
```

## Deep Dive
CSV는 초기 컴퓨터 시대부터 사용되었습니다. Elixir에서는 CSV와 함께 작업하기 위한 전용 라이브러리도 있지만, 여기서는 표준 라이브러리만 사용했습니다. 데이터 사이즈, 복잡성, 성능 요구 사항에 따라 `CSV`, `NimbleCSV` 등의 라이브러리를 고려할 수도 있습니다.

## See Also
- Elixir 공식 문서: https://elixir-lang.org/docs.html
- Hex 패키지 관리자(CSV 라이브러리): https://hex.pm
- `CSV` 라이브러리: https://hex.pm/packages/csv
- `NimbleCSV` 라이브러리: https://hex.pm/packages/nimble_csv
