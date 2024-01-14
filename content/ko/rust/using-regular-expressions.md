---
title:    "Rust: 정규 표현식 사용하기"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규식을 사용하는 이유는 텍스트에서 패턴을 찾기 위함입니다. 예를 들어, 이메일 주소를 찾는다거나 웹사이트에서 특정 단어를 찾는 경우 등에 자주 사용됩니다.

## 방법

우선 정규식 라이브러리를 프로젝트에 추가해야 합니다. `regex` 라이브러리를 사용할 것입니다. 다음은 간단한 코드 예시입니다:

```Rust
extern crate regex;

use regex::Regex;

fn main() {
    // 텍스트에 있는 이메일 주소 찾기
    let re: Regex = Regex::new(r"\w+@\w+\.com").unwrap();

    let email: &str = "example@gmail.com";

    if re.is_match(email) {
        println!("이메일 주소를 찾았습니다!");
    } else {
        println!("이메일 주소를 찾지 못했습니다.");
    }
}
```

출력:

```
이메일 주소를 찾았습니다!
```

## 딥 다이브

보다 복잡한 정규식을 사용하는 경우, 다음과 같은 팁을 알아두면 유용합니다:

- `.` : 임의의 한 문자를 의미합니다.
- `*` : 앞에 오는 패턴이 0번 이상 반복됨을 의미합니다.
- `+` : 앞에 오는 패턴이 1번 이상 반복됨을 의미합니다.
- `?` : 앞에 오는 패턴이 0번 또는 1번 나타남을 의미합니다.
- `^` : 문자열의 시작을 의미합니다.
- `$` : 문자열의 끝을 의미합니다.

위의 예시 코드에서 `is_match()` 대신 `find()` 메서드를 사용하면 텍스트에서 실제로 일치하는 패턴을 찾을 수 있습니다. 또한, `captures()` 메서드를 사용하면 정규식에서 지정한 그룹을 추출할 수 있습니다.

더 자세한 정보는 Rust 공식 문서에서 확인할 수 있습니다.

## 더 알아보기

- [Rust 정규식 문서](https://doc.rust-lang.org/std/str/struct.Regex.html) 
- [정규식 Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/rust)
- [Rust 정규식 연습 사이트](https://regexr.com/language-rust)