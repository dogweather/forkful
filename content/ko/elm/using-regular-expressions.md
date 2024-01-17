---
title:                "정규 표현식 사용하기"
html_title:           "Elm: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?
정규 표현식을 사용하는 것은 문자열에서 특정 패턴을 찾아내는 방법입니다. 프로그래머들은 이를 자주 사용하는 이유는 문자열 처리가 필요한 다양한 경우에 유용하기 때문입니다.

## 방법:
Elm에서 정규 표현식을 사용하는 과정은 간단합니다. 문자열에 매칭되는 패턴을 정의하고, 이를 활용하여 문자열을 검색하거나 수정할 수 있습니다. 예를 들어, 다음 코드는 문자열에서 숫자만 추출하는 예제입니다.
```Elm
import Regex

Regex.find Regex.AllNumbers "Hello world 123" == Just "123"
```

## 자세히 살펴보기:
정규 표현식은 1950년대에 개발된 이후로 컴퓨터 지원 언어에서 많이 사용되었습니다. 현재는 문자열 처리 뿐만 아니라 데이터 유효성 검사 등에서도 널리 사용되고 있습니다. 다른 언어에서도 정규 표현식을 사용할 수 있지만, Elm의 표현식은 간결하고 강력한 장점을 가지고 있습니다. 정규 표현식은 문자열에 대한 변경을 하기 위해 많은 반복 작업을 필요로 하는 다른 방식과 비교할 때 더욱 효율적입니다.

## 관련 자료:
- [Elm 정규 표현식 공식 문서](https://package.elm-lang.org/packages/elm/regex/latest/)
- [정규 표현식 소개 영상](https://www.youtube.com/watch?v=8CEYlKUCN-k)
- [정규 표현식 연습문제 사이트](https://regexr.com/)