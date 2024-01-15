---
title:                "CSV 작업하기"
html_title:           "Rust: CSV 작업하기"
simple_title:         "CSV 작업하기"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜 CSV를 다루는 것이 좋을까?

CSV(comma-separated values)는 데이터를 쉽게 조작하고 저장할 수 있는 형식으로 많은 사람들이 사용합니다. Rust는 그 특성을 이용하여 CSV를 다루는 것이 더욱 쉽고 효율적일 뿐만 아니라 메모리 관리도 뛰어나기 때문에 많은 사람들이 유용하게 활용하고 있습니다.

## 어떻게 Rust로 CSV 다루기
```Rust
use csv;

fn main() {
    let mut reader = csv::Reader::from_path("data.csv").expect("Failed to open CSV file");

    for result in reader.records() {
        let record = result.expect("Failed to parse record");

        // do something with the record
        println!("Name: {}, Age: {}", record[0], record[1]);
    }
}
```
위 코드는 `csv` 라이브러리를 이용하여 CSV 파일을 열고, 각 레코드를 읽어오는 간단한 예시입니다. `rust-csv` 라이브러리는 사용하기 쉬운 인터페이스를 제공하여, 개발자가 쉽게 CSV를 다룰 수 있도록 도와줍니다.

코드를 실행하면 다음과 같은 결과가 나타납니다.
```
Name: John, Age: 28
Name: Jane, Age: 35
Name: Adam, Age: 43
```

## 더 깊이 파헤쳐보기

CSV 파일을 다룰 때 주의해야 할 점은 구분자(delimiter)를 제대로 설정하는 것입니다. 보통 쉼표 혹은 탭으로 구분되기 때문에 `Reader` 객체를 생성할 때 `csv::Reader::from_path` 메서드의 두 번째 인자로 구분자를 설정할 수 있습니다. 또한 `csv` 라이브러리는 각 레코드를 벡터로 저장하기 때문에, 특정 필드를 가져오기 위해서는 해당 인덱스에 접근해야 합니다.

## 참고 자료
- [Rust csv 라이브러리 문서](https://docs.rs/csv)
- [Rust에서 CSV 파일 다루기](https://broothie.com/blog/csv-rs/)