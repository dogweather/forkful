---
date: 2024-01-26 03:42:01.967313-07:00
description: "Rust\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C\
  \ \uC81C\uAC70\uD558\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130 \uC8FC\
  \uC704\uC5D0 \uAC10\uC2F8\uC838 \uC788\uC744 \uC218 \uC788\uB294 \uBD88\uD544\uC694\
  \uD55C \uCD94\uAC00 \uB530\uC634\uD45C \uBB38\uC790\uB97C \uC81C\uAC70\uD558\uB294\
  \ \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uD30C\uC77C\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD55C\
  \ \uD6C4\uB098 \uB2E4\uB978 \uD615\uC2DD\uC73C\uB85C \uC900\uBE44\uD560 \uB54C \uB530\
  \uC634\uD45C\uAC00 \uBB38\uC81C\uAC00 \uB418\uAC70\uB098 \uC911\uBCF5\uB420 \uC218\
  \ \uC788\uAE30 \uB54C\uBB38\uC5D0 \uBB38\uC790\uC5F4\uC744\u2026"
lastmod: '2024-03-11T00:14:28.818457-06:00'
model: gpt-4-0125-preview
summary: "Rust\uC5D0\uC11C \uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C\uB97C\
  \ \uC81C\uAC70\uD558\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8 \uB370\uC774\uD130 \uC8FC\
  \uC704\uC5D0 \uAC10\uC2F8\uC838 \uC788\uC744 \uC218 \uC788\uB294 \uBD88\uD544\uC694\
  \uD55C \uCD94\uAC00 \uB530\uC634\uD45C \uBB38\uC790\uB97C \uC81C\uAC70\uD558\uB294\
  \ \uAC83\uC5D0 \uAD00\uD55C \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\
  \uB4E4\uC740 \uD30C\uC77C\uC5D0\uC11C \uB370\uC774\uD130\uB97C \uD30C\uC2F1\uD55C\
  \ \uD6C4\uB098 \uB2E4\uB978 \uD615\uC2DD\uC73C\uB85C \uC900\uBE44\uD560 \uB54C \uB530\
  \uC634\uD45C\uAC00 \uBB38\uC81C\uAC00 \uB418\uAC70\uB098 \uC911\uBCF5\uB420 \uC218\
  \ \uC788\uAE30 \uB54C\uBB38\uC5D0 \uBB38\uC790\uC5F4\uC744\u2026"
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Rust에서 문자열에서 따옴표를 제거하는 것은 텍스트 데이터 주위에 감싸져 있을 수 있는 불필요한 추가 따옴표 문자를 제거하는 것에 관한 것입니다. 프로그래머들은 파일에서 데이터를 파싱한 후나 다른 형식으로 준비할 때 따옴표가 문제가 되거나 중복될 수 있기 때문에 문자열을 정리하거나 정규화할 필요가 있을 때 이 작업을 합니다.

## 방법:

```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hello, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // 출력: Hello, Rustaceans!
}
```

가끔 이렇게 서로 다른 따옴표가 섞인 문자열이 있을 수 있습니다:

```Rust
fn main() {
    let mixed_quoted = "'Rust says: \"Hello, World!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // 출력: Rust says: "Hello, World!"
}
```

여기서는 가장 바깥쪽의 단일 따옴표만 제거됩니다.

## 깊이 들여다보기

문자열에서 따옴표를 제거할 때, 왜 단순히 `.replace("\"", "")`이 아닌지 궁금할 수 있습니다. 초창기에는 텍스트 처리가 덜 표준화되어 있었고, 다양한 시스템이 텍스트를 저장하고 전송하는 다른 방법을 가지고 있었으며, 종종 특수 문자에 대한 '이스케이프 시퀀스'가 있었습니다. Rust의 `trim_matches` 메소드는 여러 문자를 지정하여 제거하고, 문자열의 시작(접두사), 끝(접미사) 또는 양쪽을 자를지 여부를 지정할 수 있어 더 다재다능합니다.

물론 대안도 있습니다. Regex는 복잡한 패턴을 일치시키는 능력을 가진 문자열 조작의 강력한 도구이며, 단순히 따옴표를 제거하기에는 과한 수 있습니다. `trim_in_place`과 같은 라이브러리는 새로운 `String` 객체를 생성하는 오버헤드 없이 제자리에서 자르기를 제공할 수 있으며, 성능이 중요한 애플리케이션에 바람직할 수 있습니다.

내부적으로 `trim_matches`은 실제로 문자열의 양 끝에서 문자를 반복하여 제공된 패턴과 일치하지 않는 문자가 발견될 때까지 확인합니다. 그것이 하는 일에 대해 효율적이지만, 유니코드 스칼라 값으로 작업하고 있다는 점을 항상 인식해야 합니다. 문자열에 다중 바이트 유니코드 문자가 포함될 수 있다면, 이것이 그것들을 분할하지 않을 것이라고 걱정할 필요가 없습니다.

## 참고

- 문자열 조작에 대한 Rust의 문서: https://doc.rust-lang.org/book/ch08-02-strings.html
- 복잡한 패턴에 대한 `regex` 크레이트: https://crates.io/crates/regex
- 실제 코딩 시나리오에 대한 Rust by Example: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
