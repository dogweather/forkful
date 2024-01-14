---
title:                "Elm: 문자열 연결하기"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜 

문자열을 연결(concatenate)하는 것이 왜 중요할까요? Elm 프로그래밍을 할 때 문자열 연결은 꼭 필요한 기술입니다. 예를 들어, 사용자의 이름과 인사말을 합쳐서 보여주는 경우에는 문자열을 연결하는 것이 필수적입니다. 

## 어떻게 

```Elm
-- 사용자의 이름과 인사말을 합쳐보자
greet : String -> String
greet name =
    "안녕하세요, " ++ name ++ "씨!"
    
-- 출력 예제
greet "박동민" -- 결과: 안녕하세요, 박동민씨!
```

Elm에서 문자열을 연결하는 방법은 간단합니다. ++ 기호를 사용해서 문자열을 합치는 것이 가능합니다. 또한, 문자열 뿐만 아니라 다른 데이터 타입도 함께 연결할 수 있습니다. 

## 깊게 들어가기

문자열을 연결하는 것은 단순한 기술처럼 보일 수 있지만, 그 안에는 많은 로직이 포함될 수 있습니다. 예를 들어, 여러 개의 문자열을 한번에 연결하는 경우에는 어떻게 해야 할까요? 이런 복잡한 로직은 List 라이브러리를 사용해서 해결할 수 있습니다. 또한, 문자열 내부에서도 특정한 패턴을 찾거나 분리하는 작업을 수행할 수 있습니다. 

## 연관 링크

- [Elm 문자열 연산 - Elm Guide 번역](https://guide.elm-lang.org/core_language.html#string-operations)
- [Elm 문자열 연결 함수 - String.concat](https://package.elm-lang.org/packages/elm/core/latest/String#concat)
- [Elm List 라이브러리 - List 모듈](https://package.elm-lang.org/packages/elm/core/latest/List)