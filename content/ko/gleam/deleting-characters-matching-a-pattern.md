---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Gleam: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Gleam로 문자열 패턴 일치하는 문자를 삭제하는 방법

## 무엇 & 왜?
문자열에서 일치하는 패턴을 삭제하는 것은 프로그래머들이 자주 하는 작업 중 하나입니다. 일반적으로, 문자열에서 특정 패턴을 찾아서 삭제하는 것은 문자열 처리를 더욱 쉽게 만들어주기 때문입니다. 예를 들어, 데이터베이스에서 특정 패턴을 가진 문자열을 필터링하거나, 다른 문자열에서 일치하는 패턴을 제거하거나, 원하는 문자열만 추출하기 위해 사용할 수 있습니다.

## 사용 방법:
```Gleam ...
import gleam/strings

"abcd" |> strings.replace(fn c -> _ == "b") |> strings.to_list
["a", "c", "d"]
```

```Gleam ...
import gleam/strings

"Hello, World!" |> strings.replace(fn c -> c == " " || c == "-" || c == "!" ) |> strings.to_list
["Hello", "World"]

```

## 깊이있게 살펴보기:
글림(ㅎ에서는 문자열 패턴 일치를 위해 내장된 문자열 라이브러리를 사용할 수 있습니다. 또한, 함수형 프로그래밍 언어의 특성을 활용해 간단하게 문자열에 대한 작업을 수행할 수 있습니다. 이와 비슷한 결과를 얻기 위해, 정규표현식을 사용할 수도 있지만, 정규표현식을 사용하는 것은 긴 문법을 가지고 있어 초보자에게는 복잡하고 어려울 수 있습니다. 또한, 글림에서는 문자열 패턴 일치에 대해 더욱 간결하고, 실수를 더 적게 하도록 유도하기 때문에 정규표현식보다 더 나은 선택이 될 수 있습니다.

## 관련 자료:
더 많은 글림 관련 정보를 알고 싶다면 아래 링크를 참고해보세요.

- [글림 공식 홈페이지](https://github.com/gleam-lang/gleam)
- [다양한 글림 예제](https://github.com/gleam-lang/examples)
- [글림 커뮤니티 포럼](https://forum.gleam.run/)

해당 기술을 사용해 여러분의 코드를 더욱 간결하게 만들어보세요!