---
title:                "날짜를 문자열로 변환하기"
html_title:           "Haskell: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

 날짜를 문자열로 변환하는 것은 날짜와 관련된 정보를 문자열 형태로 나타내는 것을 말합니다. 프로그래머들은 이를 자주 사용하는데, 예를 들면 날짜를 데이터베이스에 저장하거나, 사용자에게 보여줄 때 등 다양한 상황에서 유용하게 사용할 수 있습니다.

## 방법:

Haskell에서 날짜를 문자열로 변환하는 방법은 `Data.Time` 모듈의 `formatTime` 함수를 사용하는 것입니다. 아래는 날짜를 다양한 형식의 문자열로 변환하는 간단한 예제입니다.

```Haskell
import Data.Time

formatTime defaultTimeLocale "%Y-%m-%d" (fromGregorian 2021 7 3)
-- 결과: "2021-07-03"

formatTime defaultTimeLocale "%Y년 %m월 %d일" (fromGregorian 2021 7 3)
-- 결과: "2021년 07월 03일"
```

## 깊이 파고들기:

`formatTime` 함수는 Haskell의 `TimeFormat` 타입을 기반으로 작동합니다. 이를 통해 우리는 다양한 날짜와 시간 형식을 제공할 수 있습니다. 또한 `formatTime` 함수는 시간대(Timezone)와 로케일(Locale)을 인자로 받을 수 있어서, 다양한 언어나 지역에 맞는 문자열을 생성할 수 있도록 해줍니다. 더 자세한 정보는 [공식 문서](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)를 참조해주세요.

만약 Haskell 이외의 다른 언어를 사용한다면, [Moment.js](https://momentjs.com/)와 같은 라이브러리를 사용할 수도 있습니다. 또한 [ISO 8601](https://ko.wikipedia.org/wiki/ISO_8601)이나 [Julian calendar](https://ko.wikipedia.org/wiki/율리우스력) 등 다양한 날짜와 시간 형식이 존재합니다.

## 관련 정보:

- [Haskell `Data.Time` 모듈 공식 문서](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Haskell `Data.Time.Format` 모듈 공식 문서](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html)
- [Moment.js 공식 사이트](https://momentjs.com/)