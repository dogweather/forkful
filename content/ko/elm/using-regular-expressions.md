---
title:    "Elm: 정규 표현식 사용하기"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜?

정규 표현식을 사용하는 이유는 무엇일까요? 간단히 말하면, 이는 문자열 내에서 특정 패턴을 찾아내기 위한 강력한 도구입니다. 이를 이용하면 복잡한 문자열 작업을 더 쉽게 처리할 수 있습니다.

## 어떻게 사용할까요?

Elm에서 정규 표현식을 사용하는 방법을 알아보겠습니다. 먼저 ```Regex``` 모듈을 임포트하고, 기본적인 정규 표현식 패턴을 정의합니다. 그리고 이를 이용해 문자열에서 패턴을 찾아내는 함수를 작성합니다.

```elm
import Regex

-- 패턴을 찾아내는 함수
findPattern : String -> Maybe (List Match)
findPattern str =
    Regex.find (Regex.regex "\d{3}-\d{4}") str

-- 예시 문자열
exampleString = "Elm은 정말 멋진 언어입니다. 저는 010-1234-5678로 연락할 수 있습니다."

-- 함수 실행 후 결과 출력
findPattern exampleString
-- 결과: Just [Match "010-1234"]
```

위 예제에서 볼 수 있듯이, 정규 표현식 패턴을 작성하고 이를 이용해 문자열에서 원하는 정보를 추출할 수 있습니다.

## 더 알아보기

정규 표현식은 강력한 도구이지만, 많은 사람들이 어렵다고 느끼는 부분도 있습니다. 이를 극복하기 위해서는 더 깊이있는 학습이 필요합니다. 여러분은 다른 자료와 예제를 참고해서 더욱 많은 패턴을 익히고, 정규 표현식을 잘 활용할 수 있게 될 것입니다.

## 더 많은 정보

- [Elm 공식 문서 - 정규 표현식](https://package.elm-lang.org/packages/elm/regex/latest/)
- [정규 표현식 테스트 도구](https://regexr.com/)
- [정규 표현식 입문서](https://regexone.com/)
- [정규 표현식 책 추천](https://emre.me/c/top-regular-expression-books/)
- [정규 표현식 예제 모음](https://www.regular-expressions.info/examples.html)

## 더 알아보기

더 많은 정보를 찾고 싶다면, 아래 페이지를 참고해보세요.

[GitHub 저장소](https://github.com/elm-lang/regex)