---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 사용하는가?

문자열 보간이란 시퀀스의 변수를 문자열 텍스트로 삽입하는 작업입니다. 프로그래머는 코드의 유지관리를 용이하게 하기 위해 이를 사용하며, 이를 통해 문자열 내에서 변수의 값을 사용하거나 업데이트할 수 있습니다.

## 어떻게 사용하는지:

다음은 Elixir에 의해 문자열 보간이 어떻게 이루어지는지 그 코드 예시와 출력을 보여줍니다.

```Elixir 
name = "John"
IO.puts("Hello, #{name}")
```

위 코드의 결과는 다음과 같습니다:

```
Hello, John
```

변수 name의 값 "John"이 문자열로 보간되어 출력되었습니다.

## 깊게 보기:

문자열 보간은 많은 프로그래밍 언어, Elixir를 포함하여 그 기원을 찾을 수 있는 기능입니다. 
대안으로, 연결 연산자를 사용하여 문자열과 변수를 병합할 수도 있지만, 가독성이 감소하고 코드가 복잡해질 수 있습니다. Elixir에서는 보간을 수행할 때 `#{}`를 사용하며, 이것이 위의 코드에서 이 변수를 문자열로 삽입하는 특별한 구문입니다.

## 참고 사항:

문자열 보간에 대한 추가 정보를 원하신다면, 다음 링크를 참조해주세요:
- Elixir 공식 문서: [String Interpolation](https://hexdocs.pm/elixir/String.html#module-interpolation)
- Elixir School: [String Interpolation](https://elixirschool.com/en/lessons/basics/strings/#string-interpolation)