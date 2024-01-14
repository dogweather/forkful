---
title:    "Gleam: 문자열을 소문자로 변환하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Gleam으로 스트링을 소문자로 변환하는 이유

Gleam은 강력하고 효율적인 함수형 프로그래밍 언어로, 다양한 문자열 처리 기능을 제공합니다. 그 중에서도 스트링을 소문자로 변환하는 기능은 매우 중요합니다. 왜냐하면 소문자는 대문자와 비교하여 더 쉽게 처리하고 분석할 수 있기 때문입니다. 따라서 Gleam을 사용하는 개발자들은 스트링을 소문자로 변환하여 더욱 유용하게 활용할 수 있습니다.

## How To

스트링을 소문자로 변환하는 방법은 매우 간단합니다. Gleam의 내장 함수 중 하나인 `String.to_lower`를 사용하면 됩니다. 다음은 예시 코드와 그에 따른 출력 결과입니다.

```Gleam
let string = "THIS IS A STRING"
let result = String.to_lower(string)
```

출력:

```sh
this is a string
```

이처럼 `String.to_lower` 함수는 모든 대문자를 소문자로 변환해주는 기능을 수행합니다. 또한, `String.to_lower/2`와 같이 두 번째 매개변수로 언어별 변환 규칙을 설정할 수도 있습니다.

## Deep Dive

`String.to_lower` 함수는 실제로 `unicode` 모듈 내에 `to_lower` 함수를 호출해 소문자로 변환합니다. 이 함수는 UTF-8 인코딩을 기준으로 한 문자를 다른 UTF-8 코드로 변환합니다. 이를 통해 다국어 지원과 문자의 대소문자 관계를 보존하여 올바른 소문자 변환이 가능합니다. 또한, `String.to_lower`는 다른 언어에서도 동작이 일관성 있게 유지될 수 있도록 `unicode` 모듈에서 표준화된 함수를 사용하고 있습니다.

## See Also

- [`String.to_lower` 함수 문서 (번역)](https://lang.gleam.run/std/string.html#to_lower)
- [Gleam 소개 및 기초 가이드 (번역)](https://gleam.run/getting-started.html)
- [Gleam 공식 문서 (번역)](https://lang.gleam.run/)