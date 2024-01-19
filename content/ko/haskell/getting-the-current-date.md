---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 받아오는 것은 현재 시각을 알아내는 과정입니다. 프로그래머들은 이를 사용해 시간에서 다양한 기능을 구현합니다. 

## 구현 방법:

Haskell에서 현재 날짜를 받아오기 위해, 우리는 다음의 라이브러리를 사용합니다: `Data.Time`

```Haskell
import Data.Time

main = getCurrentTime >>= print
```

위의 코드를 실행하면 출력값으로 현재 시각이 나옵니다.

```Haskell
2021-10-14 17:42:00.6138317 UTC
```

## 깊게 알아보기

날짜와 시간에 대한 작업은 많은 컴퓨팅 업무에서 중요한 부분을 차지하고 있으며, Haskell은 `Data.Time` 라이브러리를 통해 이를 제공합니다. 이 라이브러리는 2004년에 추가되었으며, 문서화가 잘 되어 있습니다.

Haskell에서 날짜를 얻는 다른 방법으로는, 시스템의 로컬 타임을 바로 얻는 `getZonedTime` 함수가 있습니다.

```Haskell
import Data.Time

main = getZonedTime >>= print
```

`getCurrentTime` 함수는 UTC 타임을, `getZonedTime`은 사용자의 시간대에 따라 타임을 반환합니다.

## 관련 자료

아래의 문서들에서 Haskell의 `Data.Time` 라이브러리에 대해 더 많은 정보를 얻을 수 있습니다.

- [Data.Time Documentation](https://hackage.haskell.org/package/time-1.10.0.1/docs/Data-Time.html)

- [Haskell Date and Time Guide](https://two-wrongs.com/haskell-time-library-tutorial)