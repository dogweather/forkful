---
title:                "Gleam: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜
String concatenation은 프로그래밍에서 매우 일반적인 작업입니다. 여러 개의 문자열을 하나의 큰 문자열로 결합하는 것으로, 다양한 작업에서 유용하게 사용될 수 있습니다. 예를 들어, 사용자의 이름과 성을 결합하여 사용자의 전체 이름을 생성하거나, 하나의 URL 주소를 만들기 위해 여러 문자열을 결합하는 등 다양한 상황에서 사용할 수 있습니다.

## 방법
Gleam에서 문자열을 결합하는 가장 간단한 방법은 `<>` 연산자를 사용하는 것입니다. 이 연산자를 사용하면 임의의 수의 문자열을 간단하게 결합할 수 있습니다. 예를 들어, 다음과 같이 작성할 수 있습니다.

```Gleam
let first_name = "Liam"
let last_name = "Park"
let full_name = first_name <> last_name
```

위 예제의 `full_name` 변수에는 `"LiamPark"`라는 문자열이 저장됩니다. 또 다른 예시로, 여러 개의 단어를 결합할 때는 공백을 추가해주어야 합니다.

```Gleam
let company = "Google"
let product = "Translate"
let service = "service"
let service_description = company <> " " <> product <> " " <> service
```

위 예제의 `service_description` 변수에는 `"Google Translate service"`라는 문자열이 저장됩니다.

## 깊이 들어가보기
Gleam에서 문자열을 결합할 때는 주의해야 할 사항이 있습니다. 먼저, 문자열의 데이터 유형은 `String`이지만, 다른 데이터 유형은 `concat` 함수를 사용하여 문자열로 변환할 수 있습니다. 또한 `<>=` 연산자를 사용하여 한 문자열의 끝에 다른 문자열을 추가할 수도 있습니다.

또한, 문자열 결합은 다른 연산자와 함께 사용할 수도 있습니다. 예를 들어, `<>` 연산자는 `+` 연산자와 함께 사용하여 숫자나 변수를 문자열에 결합할 수 있습니다. 또한 문자열을 조건문과 함께 사용하여 원하는 결과를 얻을 수도 있습니다.

이러한 다양한 방법으로 문자열을 결합하는 것이 Gleam에서 얼마나 유용한지 깊이 들여다보았습니다. 이제 여러분도 Gleam에서 문자열을 결합하는 데 어려움이 없을 것입니다.

## 참고
- [Gleam 공식문서](https://gleam.run/)
- [Gleam 코드 예제](https://github.com/gleam-lang/gleam/blob/main/example/arrays/src/concatenate.gleam)
- [Gleam 커뮤니티 포럼](https://forum.gleam.run/)