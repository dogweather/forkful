---
title:                "문자열 대문자로 변환하기"
html_title:           "Gleam: 문자열 대문자로 변환하기"
simple_title:         "문자열 대문자로 변환하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜
**왜 문자열을 대문자로 바꾸는 것이 유익할까요?**

문자열을 대문자로 바꾸는 것은 다양한 이유로 유용합니다. 대문자 문자열은 데이터를 정렬하거나 비교하는 데 있어서 더 적합하며, 사용자 입력을 처리하거나 데이터베이스에 쿼리를 날리는 등의 작업에서도 필요할 수 있습니다. 또한, 일부 언어에서는 대문자로 작성된 특정 단어가 문법적으로 필요할 수 있기 때문에 대문자로 변환하면 문법적인 오류를 방지할 수 있습니다.

## 하는 법
**Gleam에서 문자열을 대문자로 바꾸려면 어떻게 해야 할까요?**

우선 문자열을 변수에 할당하고, `String` 모듈의 `to_upper` 함수를 사용하여 문자열을 대문자로 변환할 수 있습니다. 아래는 Gleam으로 작성된 간단한 코드 예시입니다.

```Gleam
import String

let name = "john"
let name_uppercase = String.to_upper(name)
```

위의 코드를 실행하면, `name_uppercase` 변수의 값은 `"JOHN"`이 됩니다. 이와 같이 `to_upper` 함수는 문자열의 모든 문자를 대문자로 변환해줍니다. 또한, `to_lower` 함수를 사용하면 문자열을 모두 소문자로 변환할 수도 있습니다.

## 더 깊게 들어가기
**문자열을 대문자로 변환하는 데에는 어떤 방식이 있는 걸까요?**

일반적으로 대문자로 변환하는 방법에는 두 가지가 있습니다. 첫 번째 방법은 `String` 모듈의 함수를 사용하는 방법이며, 두 번째 방법은 문자열을 반복하여 각각의 문자를 대문자로 변환하는 방법입니다. 두 번째 방법은 알고리즘적으로 조금 더 복잡합니다만, 이를 통해 대문자로 변환되지 않아야 할 문자들을 필터링할 수 있으며, 특정 언어의 문법 규칙에 따른 대문자 변환을 수행할 수도 있습니다.

## 더 알아보기
**관련된 정보를 더 알고 싶으시다면 아래 링크들을 참고해보세요.**

- [Gleam 공식 문서](https://gleam.run/getting-started/)
- [String 모듈 문서](https://gleam.run/modules/stdlib/String.gleam)
- [문자열 다루기에 대한 다른 언어들의 라이브러리 비교](https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(strings))