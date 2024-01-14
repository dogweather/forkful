---
title:                "Gleam: 문자열 대문자로 변환하기"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 왜

자바스크립트, 파이썬과 같은 많은 메인스트림 프로그래밍 언어들은 문자열을 대문자로 변환하는 내장 함수를 제공합니다. 하지만 Gleam 언어에서는 이와 같은 기능을 제공하지 않기 때문에, 문자열을 대문자로 변환하는 방법을 배워야 합니다.

# 하는 법

문자열을 대문자로 변환하는 가장 기본적인 방법은 문자열을 반복하여 각 문자를 대문자로 변환하는 것입니다. 이를 위해 Gleam의 `String.fold` 함수를 활용할 수 있습니다. 아래의 코드를 참고해 보세요.

```Gleam
import gleam/unicode/String

pub fn capitalize(string: String) -> String {
  String.fold(string, [], fn(char, acc) -> String {
    let upper = char.to_upper()
    String.push(acc, upper)
  })
}

capitalize("hello") // "HELLO"
capitalize("gleam") // "GLEAM"
```

위의 코드를 실행하면, 입력한 문자열이 대문자로 변환되어 출력됩니다. 하지만 여전히 Gleam 언어에서 이와 같은 작업을 하는 더 간단한 방법이 있습니다. 바로 `String.uppercase` 함수를 활용하는 것입니다. 아래의 코드를 살펴보세요.

```Gleam
import gleam/unicode/String

capitalize("hello") // "HELLO"
capitalize("gleam") // "GLEAM"
```

앞서 언급한 `String.fold` 함수보다는 더 간단하지만, 이 함수 역시 입력한 문자열을 대문자로 변환해 줍니다. 또한 이 외에도 여러 가지 방법으로 문자열을 대문자로 변환할 수 있습니다. 하지만 문자열 문자를 개별적으로 변환하는 것은 그리 효율적이지 않기 때문에, 상황에 맞는 적절한 방법을 선택하는 것이 중요합니다.

# 세부 정보

이제 Gleam 언어에서 문자열을 대문자로 변환하는 기본적인 방법을 알게 되었지만, 이 외에도 다양한 기능을 활용할 수 있습니다. 예를 들어, 문자열이 포함하고 있는 모든 단어의 첫 글자만 대문자로 변환하는 기능을 구현할 수 있습니다. 또는 특정한 규칙에 따라 문자열을 대문자로 변환하는 기능 등 많은 옵션들이 있습니다. 이에 대한 더 자세한 정보를 알고 싶다면 공식 Gleam 문서를 참고해 보세요.

# 연관 정보

- Gleam 공식 홈페이지: https://gleam.run/
- Gleam 공식 문서: https://gleam.run/documentation/
- 문자열 다루기: https://gleam.run/documentation/string/