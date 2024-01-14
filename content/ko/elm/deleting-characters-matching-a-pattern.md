---
title:                "Elm: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

당신이 패턴과 일치하는 문자를 삭제하는 것에 참여하는 이유를 설명하는 1-2 문장 뿐입니다.

## 하면 된다

```Elm
-- 주어진 문자열에서 모든 숫자를 삭제하는 함수

deleteNumbers : String -> String
deleteNumbers str =
  String.filter (\c -> not (Char.isDigit c)) str
  
main =
  -- "안녕하세요! 123 Elm은 정말 멋진 언어입니다." 라는 문자열에서 숫자를 삭제한 결과는 "안녕하세요! Elm은 정말 멋진 언어입니다." 입니다.
  deleteNumbers "안녕하세요! 123 Elm은 정말 멋진 언어입니다."
  |> Debug.toString
```

## 딥 다이브

패턴과 일치하는 문자를 삭제하는 것은 효율적인 데이터 가공 및 정제 방법 중 하나입니다. 이 작업을 수행하면 데이터에서 원하지 않는 정보를 제거하고 원하는 정보만 남게됩니다. 또한 이를 통해 데이터의 일관성을 유지할 수 있습니다.

## 관련 자료

- [Elm 공식 문서의 String 모듈](https://package.elm-lang.org/packages/elm/core/latest/String)
- [RegExp를 사용하여 패턴 매칭하는 방법 알아보기](https://guide.elm-lang.org/interop/regex.html)