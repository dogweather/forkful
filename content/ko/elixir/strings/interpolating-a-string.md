---
date: 2024-01-20 17:50:32.841408-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Elixir\uC5D0\uC11C\
  \ \uBB38\uC790\uC5F4 \uB0B4\uC0BD\uC740 \uB0B4\uBD80\uC801\uC73C\uB85C `String.Chars`\
  \ \uD504\uB85C\uD1A0\uCF5C\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774\uAC83\uC740\
  \ Ruby\uC758 \uC601\uD5A5\uC744 \uBC1B\uC558\uC73C\uBA70, Elixir\uC758 \uB9E4\uD06C\
  \uB85C \uAE30\uB2A5\uC744 \uD65C\uC6A9\uD574 \uCEF4\uD30C\uC77C \uD0C0\uC784\uC5D0\
  \ \uBB38\uC790\uC5F4\uACFC \uAD00\uB828\uB41C \uAC12\uC744 \uD15C\uD50C\uB9BF \uC548\
  \uC73C\uB85C \uC0BD\uC785\uD569\uB2C8\uB2E4. \uB354 \uAC04\uB2E8\uD55C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.164681-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Elixir\uC5D0\uC11C \uBB38\uC790\
  \uC5F4 \uB0B4\uC0BD\uC740 \uB0B4\uBD80\uC801\uC73C\uB85C `String.Chars` \uD504\uB85C\
  \uD1A0\uCF5C\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (어떻게 하나요?)
```Elixir
# 변수에 기반한 문자열 내삽
name = "세상"
greeting = "안녕, #{name}!"
IO.puts greeting
# 출력: 안녕, 세상!

# 수식을 사용한 문자열 내삽
hours = 12
message = "지금은 #{hours * 2}시입니다."
IO.puts message
# 출력: 지금은 24시입니다.
```

## Deep Dive (깊이 있는 정보)
Elixir에서 문자열 내삽은 내부적으로 `String.Chars` 프로토콜을 사용합니다. 이것은 Ruby의 영향을 받았으며, Elixir의 매크로 기능을 활용해 컴파일 타임에 문자열과 관련된 값을 템플릿 안으로 삽입합니다. 더 간단한 방법으로 `<>` 연산자를 사용해 문자열을 연결할 수 있지만, 내삽은 사용하기 편하고 읽기 쉬운 코드를 작성할 수 있도록 도와줍니다.

```Elixir
# `<>` 연산자를 사용한 문자열 연결
name = "세상"
greeting = "안녕, " <> name <> "!"
IO.puts greeting
# 출력: 안녕, 세상!
```

## See Also (더 보기)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
- [Elixir Documentation: String](https://hexdocs.pm/elixir/String.html)
- [Elixir Forum for discussions](https://elixirforum.com/)
