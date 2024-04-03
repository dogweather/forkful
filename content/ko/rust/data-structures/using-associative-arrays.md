---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:06.288332-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: Rust\uC5D0\uC11C\uB294 `std::collections`\
  \ \uBAA8\uB4C8\uC758 `HashMap` \uD0C0\uC785\uC774 \uC5F0\uAD00 \uBC30\uC5F4\uC758\
  \ \uAE30\uB2A5\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uC774\uB97C\
  \ \uC0AC\uC6A9\uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:54.904338-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C\uB294 `std::collections` \uBAA8\uB4C8\uC758 `HashMap` \uD0C0\
  \uC785\uC774 \uC5F0\uAD00 \uBC30\uC5F4\uC758 \uAE30\uB2A5\uC744 \uC81C\uACF5\uD569\
  \uB2C8\uB2E4."
title: "\uC5F0\uAD00 \uBC30\uC5F4 \uC0AC\uC6A9\uD558\uAE30"
weight: 15
---

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
