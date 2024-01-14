---
title:                "Elixir: Csv와 함께 작업하기"
simple_title:         "Csv와 함께 작업하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜 CSV를 다루는 것인가

CSV 파일은 데이터를 효율적으로 저장하고 읽을 수 있는 간단한 형식입니다. 그래서 CSV 파일을 다루는 것은 데이터 분석, 데이터베이스 작업 등 다양한 프로그래밍 작업에 편리합니다.

## 방법

CSV 파일을 Elixir에서 다루는 것은 매우 쉽습니다. 먼저 `File` 모듈을 사용하여 CSV 파일을 읽고 쓸 수 있습니다.

```Elixir
values = [1, 2, 3]
File.write("numbers.csv", "#{values[0]},#{values[1]},#{values[2]}")
```

위의 예시 코드에서는 `numbers.csv`라는 파일을 만들고 `values` 변수에 저장된 값들을 CSV 형식으로 파일에 씁니다.

CSV 파일을 읽을 때는 `File.stream!` 함수를 사용하면 됩니다.

```Elixir
stream = File.stream!("numbers.csv") |> CSV.decode()

for line <- stream do
  IO.inspect(line)
end
```

위의 예시 코드에서는 `numbers.csv` 파일을 읽고, 각 라인을 `CSV.decode()`를 통해 파싱한 뒤 `IO.inspect()`를 통해 출력합니다.

## 깊게 파고들기

CSV 파일을 다룰 때 가장 중요한 것은 적절한 인코딩을 선택하는 것입니다. Elixir에서는 `:csv` 모듈을 사용하여 다양한 인코딩을 처리할 수 있습니다.

또한 `CSV` 모듈은 다양한 옵션을 제공하여 CSV 파일의 구분자(delimiter)나 인용 부호(quote) 등을 조절할 수 있습니다.

더 깊게 파고들고 싶은 분들은 Elixir 공식 문서에서 `CSV` 모듈을 참고해 보시기 바랍니다.

## 또 보기

- [Elixir CSV 공식 문서](https://hexdocs.pm/elixir/CSV.html)
- [Elixir CSV 라이브러리](https://hex.pm/packages/csv)
- [Elixir를 사용한 데이터 처리](https://queirozf.com/entries/elixir-for-huge-data-processing-csv-operations)