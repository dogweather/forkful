---
title:                "문자열 보간하기"
date:                  2024-01-20T17:50:56.826758-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 보간하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열 보간은 변수나 표현식을 문자열 중간에 삽입하는 것입니다. 이를 통해 가독성을 높이며 동적 데이터 처리가 간편해집니다.

## How to: (방법)
```gleam
fn main() {
  let name = "세계"
  let greeting = "안녕하세요, \(name)!"
  io.println(greeting)
}

// 출력: 안녕하세요, 세계!
```

## Deep Dive (심층 분석)
문자열 보간은 많은 프로그래밍 언어에 공통적으로 존재합니다. Gleam에서는 \(expression) 문법을 사용하여 구현합니다. 예전에는 문자열을 연결하기 위해 '+' 연산자 또는 `format` 함수를 사용했지만, 문자열 보간이 이를 간단하고 직관적으로 만들었습니다. 실행 시간에 문자열을 생성할 때, Gleam은 보간 표현식을 계산하고 원하는 최종 문자열로 결합합니다.

## See Also (관련 자료)
- [Gleam's official documentation on strings](https://gleam.run/book/tour/strings.html)
- [Gleam Playground](https://gleam.run)
- [Gleam's GitHub repository](https://github.com/gleam-lang/gleam)
