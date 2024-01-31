---
title:                "연관 배열 사용하기"
date:                  2024-01-30T19:13:06.288332-07:00
model:                 gpt-4-0125-preview
simple_title:         "연관 배열 사용하기"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

연관 배열 또는 러스트 사용자들이 "해시 맵"이라 부르는 것은 데이터를 키-값 쌍으로 저장하는 컬렉션입니다. 프로그래머들은 고유 키를 기반으로 효율적인 데이터 조작을 가능하게 하는 빠른 데이터 조회를 위해 이를 사용합니다.

## 사용 방법:

Rust에서는 `std::collections` 모듈의 `HashMap` 타입이 연관 배열의 기능을 제공합니다. 다음은 이를 사용하는 방법입니다:

```Rust
use std::collections::HashMap;

fn main() {
    // 새 HashMap 생성
    let mut scores = HashMap::new();

    // 값 삽입
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // 값 접근
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Blue 팀의 점수: {}", score); // 출력: Blue 팀의 점수: 10
    }

    // 값 업데이트
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // 키-값 쌍 반복 처리
    for (key, value) in &scores {
        println!("{}: {}", key, value); // 출력: Blue: 15, Yellow: 50
    }
}
```

## 심층 탐구

Rust의 `HashMap`은 키를 값에 매핑하기 위해 해싱 함수를 사용하는데, 이는 빠른 데이터 검색을 가능하게 합니다. 그러나, 이 효율성은 비용을 수반합니다: 해시 맵은 요소의 순서를 유지하지 않습니다. 이는 Python(`dict`)이나 Ruby와 같은 다른 연관 배열 구현과 대조적으로, 최근 버전에서 삽입 순서를 특징으로 유지합니다. 키-값 쌍의 순서가 중요한 사용 사례의 경우, 순서를 유지하지만 `HashMap`에 비해 삽입 및 검색 속도가 느릴 수 있는 `std::collections` 모듈의 `BTreeMap`을 사용할 수 있습니다. 결국, `HashMap`과 `BTreeMap` 사이의 선택은 순서와 성능에 대한 구체적 요구 사항에 따라 달라집니다.
