---
title:                "Elm: 날짜를 문자열로 변환하기"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜와 시간은 프로그래밍에서 중요한 요소입니다. 때로는 날짜와 시간을 다른 포맷으로 표현해야할 때가 있습니다. 이번 블로그 포스트에서는 Elm에서 날짜를 문자열로 변환하는 방법을 알아보겠습니다.

## 사용 방법

날짜를 문자열로 변환하기 위해서는 `Date` 모듈의 `toString` 함수를 사용해야 합니다. 이 함수는 두 개의 인자를 받습니다. 첫 번째 인자로는 날짜를 표현하는 `Date` 타입의 값을 전달하고, 두 번째 인자로는 원하는 포맷을 지정하는 `Date.Format` 타입의 값을 전달합니다.

```Elm
import Date exposing (Date)
import Date.Format exposing (Format)

-- 날짜를 "년-월-일" 형식으로 표현
Date.toString (Date.fromTime 1500000000) (Format.custom "yyyy-MM-dd")

-- 2017-07-14 로 출력됨
```

위의 예시 코드에서 `Date.fromTime` 함수는 타임스탬프로부터 날짜를 생성하는 함수입니다. 이를 통해 현재 시간이 아닌 다른 날짜를 사용할 수도 있습니다. 또한 `Date.Format` 모듈에는 다양한 포맷 옵션들이 있으니 필요에 따라 사용하면 됩니다.

## 더 깊게 알아보기

`toString` 함수의 두 번째 인자에 대한 값으로 `Date.Format.custom` 이외의 다른 값들을 사용하면 기본적인 날짜 형식으로 출력됩니다. 이는 `Date.Format` 모듈에 미리 정의된 값을 사용하는 것입니다. 이러한 값을 이용하면 포맷 작업을 간편하게 할 수 있습니다. 또한 `toString` 함수 외에도 `format` 함수를 통해 날짜를 문자열로 변환할 수 있습니다.

## 관련 자료

- [Elm 공식 문서 - Date 모듈](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm 공식 문서 - Date.Format 모듈](https://package.elm-lang.org/packages/elm/time/latest/Date-Format)
- [Elm 공식 문서 - Date의 `toString` 함수](https://package.elm-lang.org/packages/elm/time/latest/Date#toString)
- [Elm 공식 문서 - Date.Format의 값들](https://package.elm-lang.org/packages/elm/time/latest/Date-Format#predefined-formats)