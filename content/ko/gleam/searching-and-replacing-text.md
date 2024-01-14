---
title:                "Gleam: 텍스트 검색 및 바꾸기"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Gleam로 텍스트 검색 및 교체하는 방법

## 왜?

텍스트 검색 및 교체는 프로그래밍에서 매우 중요한 기술입니다. 이를 통해 코드나 문서에서 원하는 텍스트를 쉽게 찾고 수정할 수 있습니다. Gleam에서는 이를 위해 다양한 기능을 제공합니다.

## 어떻게 하나요?

Gleam에서는 문자열에서 특정 패턴을 찾아 교체할 수 있는 강력한 기능을 제공합니다. 예를 들어, 다음과 같은 문자열이 있다고 가정해보겠습니다.

```Gleam
let str = "Hello, world!"
```

이 문자열에서 "Hello"라는 단어를 "Hi"로 바꾸고 싶다면 다음과 같이 작성할 수 있습니다.

```Gleam
let replaced_str = String.replace(str, "Hello", "Hi")
```

그러면 "Hi, world!"라는 문자열이 출력됩니다. 또한 여러 개의 단어를 동시에 바꿀 수도 있습니다. 예를 들어, "Hello"를 "Hi"로, "world"를 "Gleam"으로 바꾸고 싶다면 다음과 같이 작성할 수 있습니다.

```Gleam
let replaced_str = String.replace(str, ["Hello", "world"], ["Hi", "Gleam"])
```

그러면 "Hi, Gleam!"이라는 문자열이 출력됩니다.

이 외에도 Gleam에서는 더 복잡한 검색과 교체를 위한 다양한 함수를 제공합니다. 자세한 내용은 아래의 "## 깊이 파보기"를 참고하시기 바랍니다.

## 깊이 파보기

Gleam에서는 검색과 교체를 위한 다양한 함수를 제공합니다. 예를 들어, 정규표현식을 사용하여 특정 패턴을 검색하고 교체할 수 있는 함수도 있습니다. 또한 대소문자를 구분하지 않는 검색이나 특정 언어의 규칙에 따라 문자열을 변환하는 함수도 있습니다. 이 외에도 Gleam에서는 검색과 교체를 위한 다양한 고급 기능을 제공하니, 깊이 파보기를 통해 더 많은 기능을 알아보시기 바랍니다.

## 더 알아보기

Gleam 공식 홈페이지: https://gleam.run/

Gleam 공식 문서: https://gleam.run/documentation

Gleam GitHub 저장소: https://github.com/gleam-lang/gleam