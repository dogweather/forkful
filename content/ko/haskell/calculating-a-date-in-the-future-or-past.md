---
title:    "Haskell: 미래나 과거의 날짜 계산하기"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

우리는 모두 인생에서 날짜를 계산하는 일에 많은 시간을 투자하게 됩니다. 결혼식, 생일, 회의 등 여러 가지 이유로 우리는 특정 날짜와 시간을 계산하고 그에 따라 행동을 취하곤 합니다. 하지만 종종 우리는 미래나 과거의 날짜를 정확하게 계산하는 것이 필요할 때가 있습니다. 이럴 때 우리는 흔히 사용하는 프로그래밍 언어 중 하나인 Haskell을 사용하여 날짜를 계산할 수 있습니다.

# 방법

우선, 미래 또는 과거의 특정 날짜를 계산하기 위해 필요한 라이브러리를 임포트해야 합니다.

```Haskell
import Data.Time
```

그 다음, 아래와 같은 함수를 사용하여 특정 날짜를 계산할 수 있습니다.

```Haskell
addDays :: Integer -> Day -> Day
```

여기서 Integer는 미래나 과거에 더할 날짜의 수이며, Day는 계산하고자 하는 날짜입니다. 예를 들어, 만약 우리가 현재 날짜에서 3일 후의 날짜를 계산하고 싶다면 아래와 같이 코딩할 수 있습니다.

```Haskell
addDays 3 (fromGregorian 2021 9 24)
```

위의 코드를 실행하면 2021년 9월 24일로부터 3일 후인 2021년 9월 27일이 계산되게 됩니다.

# 딥 다이브

더 많은 기능을 알고 싶다면 아래 레퍼런스를 참고하시기 바랍니다.

→ [Haskell의 Data.Time 라이브러리 문서](https://hackage.haskell.org/package/time-1.11.1/docs/Data-Time.html)

→ [Haskell의 날짜 계산 관련 함수 설명](https://hackage.haskell.org/package/time-1.11.1/docs/Data-Time.html#g:9)

→ [더 많은 예제 코드와 출력 결과는 여기서 확인하세요.](https://github.com/miraeyes/haskell-dates)

# 같이 보기

→ [Haskell 학습을 위한 코딩 향상 방법](https://miraeyes.github.io/learning-haskell/)

→ [데이터 구조를 이용한 Haskell 코딩 팁](https://miraeyes.github.io/haskell-data-structures/)