---
title:                "Elixir: 문자열 결합"
simple_title:         "문자열 결합"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열 연결을 하게되는 이유는 바로 더 큰 문자열을 만들기 위해서입니다. 여러 개의 작은 문자열을 엮어서 더 큰 하나의 문자열을 만들고 싶을 때, 문자열 연결은 빈번하게 사용되는 방법 중 하나입니다.

## 쉬운 방법

기본 문자열 연결 방법은 ```<>``` 기호를 사용하는 것입니다. 예를 들어, 다음과 같이 작은 두 문자열을 연결하면 더 큰 하나의 문자열이 생성됩니다.

```Elixir
"안녕하세요 " <> "저는 Jane입니다."
```

위의 예제에서는 ```<>``` 기호를 사용해서 두 개의 문자열을 연결하였습니다. 결과로는 ```안녕하세요 저는 Jane입니다.```가 출력됩니다.

```Elixir
"1" <> "2"
```

만약 숫자를 문자열로 바꿔주지 않으면, 코드에서는 오류가 발생할 것입니다. 하지만 숫자를 백틱 기호 ```#{}```로 감싸주면 자동적으로 문자열로 변환되므로 오류 없이 원하는 결과를 얻을 수 있습니다.

```Elixir
"1" <> "#{2}"
```

위의 예제에서는 ```1```과 ```2```가 문자열로 변환되어 ```"12"```가 출력됩니다.

## 깊이있는 분석

문자열 연결은 얼핏보면 간단해 보일 수 있지만, 실제로는 상당히 깊고 넓은 내용을 담고 있습니다. 문자열 연결이 내부적으로는 어떻게 동작하는지, 그리고 언제 사용해야하는지에 대해서 더 알고 싶다면 [문자열 연결 공식 문서](https://hexdocs.pm/elixir/String.html#concatenation/2)를 참고해보세요.

## 관련 링크

- [Elixir 공식 홈페이지](https://elixir-lang.org/)
- [Elixir 공식 문서](https://hexdocs.pm/elixir/)
- [Elixir School - 한국어 번역판](https://elixirschool.com/ko/)