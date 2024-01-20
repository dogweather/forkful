---
title:                "문자열을 소문자로 변환하기"
html_title:           "Bash: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 어떤 것 & 왜?

문자열을 소문자로 변환하는 것은 모든 문자들을 소문자로 바꾸는 프로세스입니다. 이것은 프로그래머들이 사용자로부터 얻은 입력을 표준화하거나, 상대적으로 대/소문자를 구별하지 않아 비교를 쉽게 만들기 위해 사용합니다.

## 어떻게 함:

Elixir에서 `String.downcase/1` 함수를 사용하여 문자열을 소문자로 변환이 가능합니다.

```Elixir
IO.puts(String.downcase("HELLO WORLD!"))
```

이 코드를 실행하면, 결과는 다음과 같습니다:

```Elixir
hello world!
```

## 디프다이브:

문자열을 소문자로 변환하는 작업은 프로그래밍의 역사만큼 오래됐고, 대부분의 언어는 이 과정을 간단하게 만들어 줍니다. Elixir에서는 'downcase()' 함수가 이 역할을 합니다.

다른 방법으로는 문자열을 반복하고 각 문자를 소문자로 변환하는 방법도 있지만, Elixir의 함수를 사용하면 이 과정을 더 쉽게 만들 수 있습니다.

Elixir에서의 소문자 변환 구현은 유니코드 호환성을 강조합니다. 따라서 모든 유니코드 문자를 손쉽게 소문자로 변환할 수 있습니다.

## 참고자료:

Elixir의 문자열을 소문자로 변경하는 방법에 대한 디테일한 문서는 아래 링크에서 찾아보실 수 있습니다:

[Elixir의 공식 문서](https://hexdocs.pm/elixir/String.html#downcase/2)