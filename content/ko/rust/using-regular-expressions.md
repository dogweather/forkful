---
title:    "Rust: 정규 표현식 사용하기"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 무엇일까요? 간단히 말하자면, 정규 표현식을 사용하면 패턴을 효과적으로 검색하고 대체하고 추출할 수 있기 때문입니다. 이를테면, 이메일 주소나 전화번호와 같은 특정한 형식의 데이터를 빠르고 정확하게 추출할 수 있습니다. 따라서 빠른 데이터 처리가 필요한 프로그래밍 작업에 유용하게 사용할 수 있습니다.

## 사용 방법

정규 표현식을 Rust로 적용하는 방법을 알아보겠습니다. 아래 예제 코드를 참고해주세요.

```Rust
// 새로운 정규 표현식 생성
let regex = Regex::new(r"(\d{3})-(\d{4})-(\d{4})").unwrap();

// 정규 표현식 매칭
let text = "010-1234-5678";
let matches = regex.captures(text);
println!("{:?}", matches);

// 결과 출력
Some(["010-1234-5678", "010", "1234", "5678"])
```

맨 위의 코드는 `Regex` 구조체를 사용해 새로운 정규 표현식을 생성하는 부분입니다. `r"(\d{3})-(\d{4})-(\d{4})"`는 전화번호 형식을 포함하는 정규 표현식입니다. 매칭을 확인하기 위해 `captures` 메소드를 사용하고 매칭 결과를 `Option<Captures>` 형태로 반환합니다. 위의 예제에서는 전화번호 형식이 매칭되어 배열 형태로 반환되는 것을 확인할 수 있습니다.

## 깊이 파고들기

정규 표현식을 사용할 때 주의해야 할 점은 성능입니다. 최적화되지 않은 정규 표현식은 매우 느린 속도로 작동할 수 있습니다. 따라서 정확한 패턴을 구성하는 것이 중요합니다. 또한 Rust의 정규 표현식 라이브러리인 `regex`는 [PCRE](https://www.pcre.org/) 라이브러리를 사용하기 때문에, PCRE의 문법을 따라야 합니다. PCRE의 문법에 대해 더 알고 싶다면, [공식 문서](https://www.pcre.org/)를 참고해주세요.

## 관련 링크

- [Rust 정규표현식 라이브러리 - regex](https://docs.rs/regex/1.3.9/regex/)
- [PCRE 공식 사이트](https://www.pcre.org/)
- [Rust 공식 사이트](https://www.rust-lang.org/)

## 참고 자료

- [Rust Programming Language Book - Regular Expressions](https://doc.rust-lang.org/book/ch09-06-pattern-matching.html)
- [Regex Tutorial - Getting Started with Regular Expressions](https://www.regextutorial.org/ko/)