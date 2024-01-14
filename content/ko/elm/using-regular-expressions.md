---
title:                "Elm: 정규식 사용하기"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 무엇일까요? 아주 간단한 이유는 우리가 문자열에서 원하는 패턴을 찾아 내기 위해서입니다. 이는 프로그래밍에서 아주 유용한 기술입니다.

## 사용 방법

만약 우리가 다음과 같은 문자열을 가지고 있다고 가정해 봅시다.

```Elm
let string = "I love coding in Elm!"
```

우리는 이 문자열에서 "love"라는 단어를 찾고 싶을 겁니다. 정규 표현식을 사용하면 이를 쉽게 할 수 있습니다.

```Elm
Regex.contains (Regex.regex "love") string
--> True
```

이 코드는 "string" 변수에 "love"라는 패턴이 포함되어 있는지를 체크합니다. 만약 해당 패턴이 있다면 "True"를 반환합니다. "Regex.regex" 함수는 문자열을 정규 표현식으로 변환하기 위해 사용됩니다.

## 깊게 파헤치기

정규 표현식을 사용하면 불필요한 반복적인 작업을 피하고 더욱 효율적인 코딩을 할 수 있습니다. 이로 인해 코드의 가독성이 향상되며 더욱 간결한 코드를 작성할 수 있습니다. 정규 표현식을 제대로 활용하면 많은 시간과 노력을 절약할 수 있습니다.

## 더 알아보기

- [Elm 정규 표현식 문서](https://elm-lang.org/docs/regex)
- [정규 표현식 샌드박스](https://ellie-app.com/new)
- [정규 표현식 연습 사이트](https://regexr.com/)