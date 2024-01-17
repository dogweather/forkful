---
title:                "정규 표현식 사용하기"
html_title:           "Haskell: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
정규표현식을 사용하는 것은 문자열을 다루거나 검색할 때 유용한 방법입니다. 프로그래머들은 정규표현식을 사용하여 문자열을 더 쉽고 빠르게 처리 할 수 있고, 패턴을 찾거나 필요한 데이터를 추출할 수 있기 때문에 일반적으로 사용합니다.

## 사용 방법:
정규표현식을 사용하려면 먼저 `regex-compat` 라이브러리가 필요합니다. 이 라이브러리를 불러오려면 다음과 같이 입력해야 합니다:

```Haskell
import Text.Regex
```

그런 다음 패턴을 정의하고 `matchRegex` 함수를 사용하여 해당 패턴을 사용할 문자열과 일치하는 경우 작업을 수행할 수 있습니다.

예를 들어, 알파벳 "a" 뒤에 오는 숫자가 2개인지 확인하기 위해서는 다음과 같이 입력할 수 있습니다:

```Haskell
let pattern = "a[0-9]{2}" -- "a" 뒤에 오는 숫자가 2개인 패턴
let text = "a12" -- 검사할 문자열
matchRegex pattern text -- 결과로 "Just [\"a12\"]"이 출력됨
```

## 더 깊게 들어가기:
정규표현식은 조금 복잡하게 느껴질 수 있습니다. 여러분은 항상 해당 패턴을 이해할 수 있는지 확인하고 싶을 수 있습니다. 하지만 이것은 처음에는 어려울 수 있지만, 기분 좋게 배우고 익히면 다루기가 더 쉬워집니다.

정규표현식은 20세기 후반에 켄 톰프슨과 로버트 파이크가 Bell Labs에서 개발했습니다. 표준 정규표현식(SRE)은 Lex 프로그램의 부분으로 시작되었고, 이후 다른 언어와 도구에 적용되었습니다.

정규표현식 대신에 문자열 처리를 위해 다른 방법을 선택할 수도 있습니다. 예를 들어, `split` 함수를 사용하여 문자열을 나눌 수도 있습니다. 하지만 이 함수는 정규표현식과 비교했을 때 한계가 있을 수 있고, 더욱 복잡한 작업을 수행할 수 없을 수 있습니다.

## 더 알아보기:
[Haskell.org: 정규표현식 사용하기](http://hackage.haskell.org/package/regex-compat)
[한글 위키피디아: 정규표현식](https://ko.wikipedia.org/wiki/정규_표현식)
[Red Hat: 문자열 처리를 위한 다른 방법 사용하기](https://www.redhat.com/ko/topics/data/regex-vs-find-vs-split)