---
title:                "Gleam: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 부분 문자열을 추출하는 것은 많은 이유로 유용합니다. 그것을 사용하여 특정 단어나 문구를 찾아내거나 문자열의 길이를 줄이는 것이 가능합니다.

## 어떻게

Gleam에서는 `String` 모듈에서 `slice` 함수를 사용하여 부분 문자열을 추출할 수 있습니다. 아래는 `slice` 함수를 이용한 예제 코드입니다.

```Gleam
let original_string = "이것은 Gleam입니다."

let substring = original_string |> String.slice(5, 10)

// 출력: "Gleam"
```

위의 예제 코드에서 `slice` 함수는 첫 번째 인자로 시작 인덱스, 두 번째 인자로 종료 인덱스를 받습니다. 그리고 해당 범위에 있는 부분 문자열을 리턴합니다.

인덱스는 0부터 시작하며, 음수 값도 가능합니다. 음수 값의 경우, 뒤에서부터 인덱싱이 이루어집니다. 예를 들어, `-1`을 인자로 줄 경우, 맨 뒤의 문자를 선택합니다.

## 깊게 들어가기

`slice` 함수에는 세 번째 인자로 스텝(step) 값을 받을 수 있습니다. 스텝 값은 기존 문자열에서 건너뛰며 부분 문자열을 구성하는데 사용됩니다. 예를 들어, `slice(0, 10, 2)`는 0부터 시작하는 인덱스에서 2칸씩 건너뛰면서 10번째 인덱스까지의 부분 문자열을 리턴합니다.

또한, Gleam에서는 부분 문자열을 변경하기 위한 `set_slice` 함수도 제공합니다. 예를 들어, `set_slice(original_string, 5, 10, "Elixir")`는 `original_string` 변수에서 5번째 인덱스부터 10번째 인덱스까지의 부분 문자열을 변경하는 것입니다.

## 관련 글들

- [Gleam 공식 문서의 String 모듈 섹션](https://gleam.run/core/string.html)
- [How to Use Gleam’s String Module](https://dev.to/kennethlum93/how-to-use-gleam-s-string-module-4e1g)