---
title:                "정규 표현식 사용하기"
html_title:           "Elixir: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 왜 정규식을 사용해야 할까요?

정규식은 많은 프로그래밍 언어에서 지원하는 강력한 문자열 처리 기능입니다. 이를 사용하면 복잡한 문자열 검색 및 대체 작업을 쉽게 수행할 수 있으며, 코드의 가독성과 유지 보수성을 높일 수 있습니다.

## 사용 방법

```Elixir
# 단어 'cat'을 포함하는 문자열 찾기
 regex = ~r/cat/
 str = "I have a big cat and a small cat"
 Regex.scan(regex, str) # 결과: ["cat", "cat"]

# 이메일 형식이 맞는지 확인
regex = ~r/^[\w+\-.]+@[a-z\d\-]+(\.[a-z]+)*\.[a-z]{2,}$/i
email = "email@example.com"
Regex.match?(regex, email) # 결과: true
```

위의 예제처럼 `~r`을 사용하여 정규식을 정의한 후, `Regex` 모듈에서 제공하는 메소드를 사용하여 문자열을 검색하거나 매칭할 수 있습니다. 또한 Elixir의 패턴 매칭 기능과 함께 사용하면 더욱 강력한 기능을 발휘할 수 있습니다.

## 더 깊게 살펴보기

정규식은 Perl에서 시작된 다양한 문법과 표현식을 지원합니다. 이를 조합하여 매우 다양한 문자열 검색 및 변형 작업을 수행할 수 있습니다. 또한 Elixir은 정규식을 더욱 효율적으로 사용할 수 있도록, JIT 컴파일러인 `Lager`를 제공하고 있습니다.

# 더 알아보기

- [Elixir 정규식 문서](https://hexdocs.pm/elixir/Regex.html)
- [정규식 연습 사이트 (영어)](https://regexr.com/)
- [정규식 생성기 (영어)](https://regex101.com/)