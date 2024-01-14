---
title:                "Gleam: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜 

문자 패턴에 맞는 문자를 삭제하는 것은 일반적인 텍스트 처리 작업이며, 이를 통해 텍스트를 정리하고 원하는 결과를 얻을 수 있습니다.

## 방법 

Gleam은 패턴 매칭 및 문자 삭제를 지원하는 내장 함수와 라이브러리를 제공합니다. 아래 코드 예제를 통해 자세한 사용 방법을 알아보세요.

```Gleam 
let text = "Hello, world!"
let pattern = ["w", "o", "r"]
let output = text |> Text.normalize |> Text.replace_all(pattern, "")
IO.inspect(output) // 출력: Hel, ! 
```

위의 코드에서, 'w', 'o', 'r'이라는 패턴에 해당하는 문자들이 모두 삭제되어 출력 결과에서는 'world'가 제외됩니다. 이를 통해 텍스트에서 원하지 않는 부분을 손쉽게 제거할 수 있습니다.

## 깊게 파고들기 

Gleam에서 문자 패턴 매칭 및 삭제는 내장 함수를 사용하는 것 이외에도 다양한 방법으로 할 수 있습니다. 예를 들어, 정규식을 사용하여 패턴을 지정하고 삭제하는 방법도 있습니다. 이를 통해 더욱 복잡한 문자열 처리를 할 수 있습니다. 또한 Gleam에서 제공하는 다양한 라이브러리도 활용하여 텍스트 처리에 더욱 다양한 기능을 추가할 수 있습니다.

## 관련 자료 

- Gleam 공식 문서: https://gleam.run/
- Gleam 패턴 매칭 함수: https://gleam.run/stdlib/text.html#replace-pattern-3
- 정규식 패턴 매칭: https://gleam.run/stdlib/regex.html
- Gleam 문자열 처리 라이브러리: https://github.com/gliesian/gleam-strings