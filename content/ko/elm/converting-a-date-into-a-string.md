---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜합니까?

날짜를 문자열로 변환하는 것은 "12월 1일, 2021"와 같이 사용자에게 친숙한 형식으로 날짜 데이터를 표시하기 위해 필요합니다. 프로그래머들은 읽기 쉬운 형태로 데이터를 표현하거나 서로 다른 시스템 간에 데이터를 교환하는 데 이를 사용합니다.

## 어떻게 사용하는가: 

이는 Elm을 사용하여 날짜를 문자열로 변환하는 방법입니다:

```Elm
import Time
import Time.Extra

toIsoString : Time.Posix -> String
toIsoString date =
    Time.toIsoString date

main = 
    let 
        date = Time.millisToPosix 1639348582000
    in
    text (toIsoString date)
```

이 코드의 출력은 다음과 같습니다: "2021-12-12T21:29:42.000Z"

## 깊이 들어가보기:

* **역사적 맥락** 
ISO (International Organization for Standardization) 8601 은 원래 1988년에 출시되어 사용자가 날짜와 시간을 쉽게 읽고 이해할 수 있도록 표준화된 날짜와 시간 형식을 제공합니다.
  
* **대안** 
Elm에서는 저수준 날짜 및 시간 처리를 제공하는 `Time` 패키지 외에도 Justgook의 `elm-date-extra`와 같이 다양한 고수준 라이브러리를 사용하여 날짜와 시간을 처리할 수 있습니다.
   
* **구현 세부 사항** 
`Time.toIsoString` 함수를 사용하여 날짜를 문자열로 변환하면, 출력된 문자열은 항상 UTC (Coordinated Universal Time)로 표시됩니다. 따라서 별도의 시간대 조정이 필요할 수 있습니다.

## 참고 자료:

1. [Elm 공식 문서](https://package.elm-lang.org/packages/elm/time/latest/)
2. [ISO 8601 Wiki](https://ko.wikipedia.org/wiki/ISO_8601)