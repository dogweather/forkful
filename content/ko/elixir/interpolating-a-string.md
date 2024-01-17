---
title:                "문자열 보간하기"
html_title:           "Elixir: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 무엇이고 왜

문자열 내삽이란 무엇인지 먼저 알아보겠습니다. 간단히 말해서, 문자열 내삽이란 문자열 안에 변수를 삽입하는 것을 의미합니다. 이 방법을 사용하는 이유는, 코드를 보다 간결하고 가독성 있게 만들기 위해서입니다. 변수를 따로 문자열로 만들지 않고 바로 삽입함으로써, 코드를 작성할 때 불필요한 공간을 절약할 수 있습니다.

## 하는 법

다음은 두 개의 변수를 문자열로 삽입하는 간단한 코드 예제입니다. 

```Elixir
string1 = "Hello"
string2 = "World"

IO.puts "#{string1}, #{string2}"
```

위 코드를 실행하면, "Hello, World"가 출력됩니다. 보다시피, 변수를 문자열로 바로 삽입함으로써 코드가 간단하고 가독성이 좋아졌습니다.

## 더 들어가기

내삽은 C언어에서 시작되어 최근 많은 다른 언어들에서도 지원하고 있습니다. 또한, 내삽을 사용하는 대체 방법으로는 string concatenation이 있습니다. 문자열을 더하는 것보다 내삽을 사용하는 것이 코드를 더 간결하고 보기 좋게 만들 수 있습니다.

내삽의 구현 방법은 각 언어마다 다르지만, 보통 코드의 내부적으로는 변수들을 문자열로 변환한 다음 문자열을 조합해서 출력하는 방식으로 이루어집니다.

## 관련 자료

[Elixir 공식문서](https://elixir-lang.org/getting-started/string-interpolation.html)에서 문자열 내삽에 대한 더 자세한 정보를 찾을 수 있습니다. 또한, 다양한 블로그와 포럼에서 내삽에 대한 다양한 사례를 찾을 수 있습니다.