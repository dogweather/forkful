---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

패턴에 일치하는 문자를 삭제하는 것은 특정 패턴의 문자를 코드에서 제거하는 프로세스입니다. 프로그래머는 코드를 깨끗하게 유지하고, 불필요한 문자를 제거하여 실행 속도를 향상시키기 위해 이를 사용합니다.

## 어떻게:

다음은 패턴에 일치하는 문자를 삭제하는 간단한 Gleam 코드 예제입니다:

```Gleam
fn delete_matching_chars(input: String, pattern: String) -> String {
  string.replace(input, pattern, "")
}

fn main() {
  let result = delete_matching_chars("Hello, Gleam!", "Gleam");
  io.println(result); // "Hello, !"
}
```

## 간략해설 

패턴에 일치하는 문자를 삭제하는 개념은 초기 프로그래밍 언어부터 있었습니다. 일부 언어(e.g., Regex)는 복잡한 문자열 패턴을 대체하는 데 특히 강력하다. 다른 gleam 함수 string.replace_all을 사용하여 동일한 결과를 얻을 수도 있습니다. 

Gleam에서 이 기능의 구현은 Elixir의 String 모듈을 기반으로 합니다. 이를 통해 빠른 문자열 치환 연산을 제공합니다.

## 참고문헌

다음 링크에서 더 많은 정보를 찾을 수 있습니다:
- [Gleam Documentation: String](https://gleam.run/book/tour/strings.html)

- [ElixirLang: String](https://hexdocs.pm/elixir/String.html)

- [Gleam GitHub](https://github.com/gleam-lang/gleam)