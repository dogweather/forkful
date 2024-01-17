---
title:                "문자열 소문자로 변환하기"
html_title:           "Elixir: 문자열 소문자로 변환하기"
simple_title:         "문자열 소문자로 변환하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 무엇과 왜?
문자열을 소문자로 변환하는 것은 일반적으로 문자열을 비교하거나 검색할 때 효율적이기 때문에 프로그래머들이 자주 사용하는 기능입니다.

# 방법:
Elixir에서 문자열을 소문자로 변환하는 방법을 살펴보겠습니다. 아래의 코드 블록을 참고하세요.
```
# 소문자로 변환하기
lower_case_string = String.downcase("HELLO WORLD")
IO.puts lower_case_string
```
출력 결과:
```
hello world
```

# 깊게 파헤치기:
소문자 변환은 대문자와 소문자의 구분이 필요한 언어에서 많이 사용됩니다. 예를 들어, 영어에서는 대문자와 소문자가 구분되기 때문에 이 기능을 사용하여 두 문자열을 비교할 때 정확한 결과를 얻을 수 있습니다.

알파벳 이외의 다른 언어를 다루는 경우, 소문자 변환 함수도 다른 언어의 문자열 규칙을 지원합니다. 문자열에 포함된 다른 언어의 문자를 소문자로 변환할 수 있습니다.

```
# 다른 언어의 문자열을 소문자로 변환하기
lower_case_string = String.downcase("HOŞGELDİNİZ")
IO.puts lower_case_string
```

출력 결과:
```
hoşgeldiniz
```

# 관련 정보:
- Elixir 공식 문서: https://hexdocs.pm/elixir/String.html#downcase/1
- Elixir 포럼: https://elixirforum.com/t/how-to-convert-string-to-lowercase-in-elixir/5615
- Stack Overflow 포스트: https://stackoverflow.com/questions/271561/how-do-i-convert-camelcase-into-human-readable-names-in-java