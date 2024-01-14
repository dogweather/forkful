---
title:                "Elm: 정규 표현식을 사용하는 방법"
simple_title:         "정규 표현식을 사용하는 방법"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 데이터를 처리하고 추출하는 데 매우 효과적이기 때문입니다.

## 어떻게

정규 표현식을 사용하기 위해서는 Elm 언어에서 제공하는 `Regex` 라이브러리를 import 해야합니다. 그 후, 아래의 예시 코드처럼 패턴을 정의해주고, `Regex.find` 함수를 사용하여 데이터에서 원하는 부분을 추출할 수 있습니다.

```Elm
import Regex

pattern = Regex.regex "[0-9]+"
text = "I have 3 apples and 5 oranges."

matches = Regex.find pattern text
```

위의 코드에서는 `"[0-9]+"` 패턴을 사용하여 `3`과 `5`라는 숫자를 추출하고 있습니다. 만약 대소문자를 구분하고 싶지 않다면 `"[0-9]+"` 대신 `(?i:[0-9]+)` 패턴을 사용하면 됩니다. 또는 매칭되는 모든 부분을 추출하고 싶다면 `Regex.findAll` 함수를 사용하면 됩니다.

추출한 결과는 `Regex.Match` 타입으로 반환됩니다. 이를 통해 추출한 데이터를 사용할 수 있습니다. 또한 `matches` 변수는 리스트 형태로 반환되므로 `List.map` 함수를 사용하여 각각의 요소를 다른 형태로 변환할 수 있습니다.

```Elm
formattedMatches = List.map Regex.matchToString matches
```

위의 예시 코드를 실행하면 `["3", "5"]`라는 리스트가 반환됩니다. 이렇게 추출한 데이터를 원하는 대로 활용할 수 있습니다.

## 깊게 들어가기

정규 표현식은 매우 강력하고 유연한 기술이지만, 사용법을 익히는 것은 쉽지 않을 수 있습니다. 패턴을 정의할 때에는 자주 사용하는 특수문자나 패턴을 반복적으로 확인해보는 것이 좋습니다. 또한 일반적으로 `Regex` 라이브러리에서는 와일드카드로 `.` 문자를 사용하지만, `Regex.custom` 함수를 사용한다면 와일드카드로 어떤 문자와도 매칭할 수 있습니다.

또한 정규 표현식은 속도가 빠르지 않은 편이기 때문에, 많은 데이터를 처리해야 할 경우에는 다른 방식을 고려해보는 것이 좋습니다.

## 더 알아보기

- [Elm Regex 문서](https://package.elm-lang.org/packages/elm/regex/latest/)
- [정규 표현식 패턴 참고 사이트](https://regexr.com/)
- [정규 표현식으로 문자열 분리하기](https://guide.elm-lang.org/interop/javascript.html#use-incoming-strings)
- [문자열 처리에 유용한 문자 함수들](https://package.elm-lang.org/packages/elm/core/latest/String)

## 더 보기

- [Markdown 문법 공식 문서](https://daringfireball.net/projects/markdown/syntax)
- [Why Elm? (한국어 번역)](https://medium.com/@marcel_cutting/why-elm-3aceb60a7713)
- [Elm 기본 개념과 공식 문서](https://dmyerin.com/2018/06/25/shorter/1020)