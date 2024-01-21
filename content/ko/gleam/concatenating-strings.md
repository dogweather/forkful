---
title:                "문자열 연결하기"
date:                  2024-01-20T17:35:07.555217-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 결합은 서로 다른 문자열을 하나로 합치는 것입니다. 프로그래머들은 데이터를 형식화하거나, 메시지를 생성하거나, 사용자 인터페이스를 구성할 때 이 기법을 사용합니다.

## How to: (방법)
```gleam
fn main() {
  let greeting = "안녕하세요, "
  let name = "지수님!"
  let message = greeting ++ name  // 문자열 결합하기
  message
}

// 출력: "안녕하세요, 지수님!"
```

기본적으로 `++` 연산자를 사용해서 문자열을 쉽게 결합할 수 있습니다.

```gleam
fn format_user(greeting: String, name: String) -> String {
  greeting ++ " " ++ name
}

fn main() {
  let result = format_user("환영합니다", "홍길동")
  result
}

// 출력: "환영합니다 홍길동"
```

함수를 만들어서 여러 문자열을 결합하는 것도 가능하죠.

## Deep Dive (심층 분석)
문자열 결합은 오래전부터 프로그래밍의 기본 요소였습니다. 기억을 최소로 사용하면서 문자열을 합치는 효율적인 방법을 개발하는 것이 중요했죠. Gleam에서는 `++` 연산자로 문자열을 결합할 수 있는데, 내부적으로는 리스트의 연결과 유사하게 처리되곤 합니다. 이는 Erlang VM 위에서 동작하고, 효율성과 안정성에 중점을 둔 방식이라고 할 수 있습니다.

다른 언어에서는 `+`, `.concat()`, `.join()` 등 다양한 메소드를 사용하여 문자열을 결합할 수 있지만, Gleam은 간단하게 `++`를 사용합니다. 이는 코드를 읽고 쓰기 쉽게 만들어주고, 다른 언어로부터 이어진 함수 양식을 유지합니다.