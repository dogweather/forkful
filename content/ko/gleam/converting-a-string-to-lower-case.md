---
title:                "스트링을 소문자로 변환하기"
html_title:           "Gleam: 스트링을 소문자로 변환하기"
simple_title:         "스트링을 소문자로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

안녕, 프로그래머 여러분!

Gleam 프로그래밍 언어에서는 문자열을 소문자로 변환하는 기능이 있습니다. 이 기능에 대해서 잠깐 알아보고, 왜 프로그래머들이 이것을 하는지에 대해 알아봅시다.

## 무엇 & 왜?

문자열을 소문자로 변환하는 것은, 문자열의 모든 대문자를 소문자로 변환하는 것을 의미합니다. 프로그래머들은 이 기능을 사용하여 문자열을 단순하게 만들고, 정확성을 보장하기 위해 사용합니다.

## 사용 방법:

```Gleam
let my_string = "HeLLo"
my_string |> String.to_lower_case
```
위의 코드는 "hello"를 출력합니다.

문자열이 아닌 다른 객체들을 변환할 수 있습니다. 예를 들어, 숫자나 불린값들도 소문자로 변환할 수 있습니다.

## 더 깊게 들어가보기:

이 기능은 미국의 인코딩 규칙인 ASCII 코드에서 시작되었습니다. 이러한 규칙을 따라야 다른 사람들이 쉽게 이해하고 사용할 수 있기 때문입니다. 다른 언어와는 다르게, Gleam에서는 이 기능이 기본적으로 내장되어 있기 때문에, 별도로 라이브러리를 불러오지 않아도 됩니다.

때때로, 대문자를 소문자로 변환하는 대안으로는, 문자열을 모두 소문자로 입력하는 방법이 있습니다. 그러나, 이렇게 하게 되면 코드의 길이가 더 길어지고, 가독성이 떨어지게 됩니다.

## 관련 소스 보기:

확인해볼 만한 다른 자료들을 아래 링크를 통해 확인해보세요.
- [Gleam 공식 문서](https://gleam.run)
- [Gleam의 String 라이브러리](https://gleam.run/libraries/standard-library/#string)