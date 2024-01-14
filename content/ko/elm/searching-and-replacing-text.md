---
title:                "Elm: 텍스트 검색 및 교체"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜

Elm에서 텍스트 검색과 대체를 하는것은 여러 이유들이 있습니다. 주요 이유는 다음과 같습니다:
1. 프로그래머들은 코드를 작성하거나 유지보수 할 때 자주 특정 텍스트를 한번에 모두 바꾸기를 원합니다.
2. 프로그래머들은 오타를 수정하거나 일괄 변경할 필요가 있을때 검색과 대체를 사용합니다.

## 어떻게

검색과 대체를 Elm에서 하기 위해서는 `String.replace` 함수를 사용할 수 있습니다. 다음과 같은 형식으로 사용할 수 있습니다:

```Elm
String.replace "찾을 문자열" "바꿀 문자열" "원본 문자열"
```

이 함수는 원본 문자열에서 찾을 문자열을 발견하면 그 문자열을 바꿀 문자열로 대체합니다. 예를 들어, `"안녕하세요"`라는 문자열에서 `"하"`를 `"하마"`로 바꾸고 싶으면 다음과 같이 쓸 수 있습니다:

```Elm
String.replace "하" "하마" "안녕하세요"
```

이 코드의 결과는 `"안녕마세요"`가 됩니다.

## 더 깊이 들어가기

Elm에는 `String.replace` 함수 이외에도 다양한 검색과 대체를 할 수 있는 함수들이 있습니다. 예를 들어, `String.replaceRegex` 함수를 사용하면 정규표현식을 이용해 더 복잡한 규칙으로 검색과 대체를 할 수 있습니다. 또한 `String.contains` 함수를 사용하면 문자열에 특정 문자열이 포함되어 있는지를 확인할 수 있습니다. 이런 함수들을 조합하면 보다 다양한 검색과 대체를 할 수 있습니다.

# 참고 자료

- [Elm 공식 사이트](https://elm-lang.org/)
- [Elm 표준 라이브러리 문서](https://package.elm-lang.org/packages/elm/core/latest/)
- [정규표현식 간단히 알아보기](https://velog.io/@hss930622/Regexp%EC%9D%80-%EB%AD%90-%EB%82%B4-%EC%84%A4%EB%AA%85-%EC%9E%85%EB%A0%A5-%EB%93%B1%EB%A1%9D%EC%9D%84-%EC%9C%84%ED%95%B4%ED%95%98%EB%8A%94-%EC%84%A4%EB%AA%85)
- [정규표현식 메타문자 참고 사이트](https://metadocs.net/re2/master/01-introduction.md.html#metacharacters)