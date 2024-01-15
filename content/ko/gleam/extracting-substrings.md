---
title:                "부분 문자열 추출"
html_title:           "Gleam: 부분 문자열 추출"
simple_title:         "부분 문자열 추출"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?
뭔가 문자열의 일부분만 추출하는 작업은 매우 기본적이지만, 이 작은 기능이 여러 가지 다른 작업을 수행하는데 매우 유용하게 사용될 수 있습니다. 예를 들어, 문자열에서 특정 단어를 찾아 바꾸거나, 지정된 패턴에 맞는 문자열을 추출하는 등의 경우에 문자열 추출 함수를 사용할 수 있습니다.

## 어떻게?
우선, `import` 문을 사용하여 `gleam/strings` 라이브러리를 가져옵니다. 그리고 문자열을 선언합니다. 이번 예제에서는 "Hello world!" 문자열을 사용하겠습니다.

```gleam
import gleam/strings

let hello_world = "Hello world!"
```

이제 문자열 추출 함수를 사용하여 문자열의 일부분을 추출해보겠습니다. `gleam/strings` 라이브러리에서 제공하는 `slice` 함수를 사용하면 간단하게 문자열을 추출할 수 있습니다. `slice` 함수에는 세 가지 매개변수가 있습니다: 추출할 문자열, 추출할 시작 인덱스, 추출할 끝 인덱스입니다. 시작 인덱스와 끝 인덱스는 모두 0부터 시작하며, 추출할 인덱스는 끝 인덱스의 바로 앞까지 추출됩니다. 예를 들어, "ello" 문자열을 추출하려면 시작 인덱스와 끝 인덱스를 각각 1과 5로 설정하면 됩니다.

```gleam
gleam/strings.slice(hello_world, 1, 5)
```

위 코드를 실행하면 "ello" 문자열이 출력됩니다. 또한, 시작 인덱스와 끝 인덱스 대신 음수 값을 사용하여 뒤에서부터 추출할 수도 있습니다. 예를 들어, 문자열의 마지막 세 글자를 추출하려면 다음과 같이 음수 값을 사용합니다.

```gleam
gleam/strings.slice(hello_world, -3, -1)
```

위 코드를 실행하면 "ld" 문자열이 출력됩니다.

## 깊이 파고들기
여러 가지 코드를 작성해보고, 다양한 인덱스를 사용하여 문자열을 추출해보면서 `gleam/strings` 라이브러리에는 `slice` 함수 외에도 유용한 문자열 처리 함수들이 많이 있다는 것을 알 수 있습니다. 이를 통해 문자열 추출 함수를 항상 최적의 방법으로 사용할 수 있으며, 앞으로도 이러한 유용한 함수들을 적극적으로 활용하여 더 나은 코드를 작성할 수 있을 것입니다.

## 더 알아보기
더 많은 문자열 처리 함수들을 살펴보고 싶다면 아래 링크를 참고해보세요.

- [Gleam Strings 라이브러리 문서](https://gleam.run/core/gleam/strings/)
- [Gleam 공식 홈페이지](https://gleam.run/)