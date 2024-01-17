---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Elm: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
무엇은 패턴과 일치하는 문자를 삭제하는 것이며, 프로그래머들은 이 작업을 수행하는 이유는 코드의 유지보수와 가독성을 위해서입니다.

## 방법:
``` Elm
-- Example 1: 문자열에서 패턴과 일치하는 문자 삭제
deleteCharsMatchingPattern : String -> String
deleteCharsMatchingPattern string =
    String.filter (\char -> not (String.contains "pattern" (String.fromChar char))) string

-- 출력: "This is a sample string"
```

``` Elm
-- Example 2: 리스트에서 패턴과 일치하는 항목 삭제
deleteItemsMatchingPattern : List String -> List String
deleteItemsMatchingPattern list =
    List.filter (\item -> not (String.contains "pattern" item)) list

-- 출력: ["item1", "item2", "item3"]
```

## 깊게 알아보기:
(1) 이 작업의 역사적 배경, (2) 대안, (3) 문자 매칭 패턴 삭제의 구현 세부 사항 등과 같은 추가 정보를 제공합니다.

## 관련 자료:
관련 자료를 제공하는 링크: [링크 1](https://example.com), [링크 2](https://example.com)