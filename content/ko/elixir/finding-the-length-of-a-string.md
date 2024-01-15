---
title:                "문자열의 길이 찾기"
html_title:           "Elixir: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 길이를 찾는 것을 왜 해야 할까요? 그것은 간단합니다. 우리는 때때로 문자열의 길이를 알고 싶을 때가 있습니다. 예를 들어, 사용자의 입력을 검증하거나 특정한 조건을 만족하는지 확인할 때 문자열의 길이가 중요합니다.

## 어떻게

 ```elixir
string = "안녕하세요"
IO.inspect String.length(string)
```

위 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
7
```

문자열의 길이를 찾는 가장 간단한 방법은 `String.length/1` 함수를 사용하는 것입니다. 이 함수는 주어진 문자열의 길이를 반환합니다. 또한, 문자열의 길이를 알아내기 위해 내장 함수를 사용하지 않고도 직접 구현할 수도 있습니다. 예를 들어, 문자열을 리스트로 변환한 후 `Enum.count/2` 함수를 사용하여 길이를 계산할 수 있습니다. 하지만 내장 함수를 사용하는 것이 더 쉽고 효율적입니다.

## 깊이 파고들기

문자열의 길이를 알아내는 것은 Elixir의 내장 함수 중 일부입니다. 이 함수들은 문자열을 다루는 데 도움이 되는 다양한 기능을 제공합니다. 예를 들어 `String.split/3` 함수는 문자열을 주어진 구분자를 기준으로 나누어 리스트로 반환해줍니다. 또한, `String.upcase/1` 함수를 사용하면 문자열을 모두 대문자로 변환할 수 있습니다.

## 더 많은 정보

Elixir의 문자열 함수에 대해 더 자세한 정보가 궁금하다면 공식 문서를 참고하세요. 또한, `String` 모듈의 소스 코드를 살펴보면 내부적으로 어떻게 작동하는지 알 수 있습니다.

## 더 알아보기

- [Elixir 공식 문서](https://hexdocs.pm/elixir/String.html) 
- [Elixir 문자열 함수 실습](https://elixirschool.com/kr/lessons/basics/string-functions/)
- [Elixir 문자열 처리 팁](https://medium.com/@anildigital/elixir-supercharge-your-string-processing-tips-31ce30033986)