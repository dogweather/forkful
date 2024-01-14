---
title:                "Elm: 두 날짜 비교하기"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 왜
두 날짜를 비교하는 것에 참여하는 이유는 무엇인가요? 

두 날짜를 비교하는 것은 많은 프로그래밍 언어에서 자주 사용되는 공통적인 작업 중 하나입니다. 이는 특히 날짜와 시간 정보를 처리해야 하는 웹 애플리케이션을 개발할 때 매우 유용합니다. Elm에서는 간단하게 두 날짜 간의 차이를 계산하고 처리할 수 있으므로 이를 기준으로 다양한 예제를 살펴보겠습니다.

# 방법

Elm에서 두 날짜를 비교하는 것은 매우 간단합니다. 먼저 `elm/time` 모듈을 가져와야 합니다.

```Elm
import Time
```

그리고 비교할 두 날짜를 변수로 선언해야 합니다. 여기서는 `start`와 `end` 두 변수를 사용할 것입니다.

```Elm
start : Time.Posix
start = Time.millisToPosix 1624656318 -- 2021년 6월 26일 오전 1시 58분 38초

end : Time.Posix
end = Time.millisToPosix 1624659951 -- 2021년 6월 26일 오전 3시 5분 51초
```

이제 두 날짜 간의 차이를 계산하려면 `Time.diff` 함수를 사용하면 됩니다.

```Elm
diff : Time.Posix -> Time.Posix -> Time.Span
diff start end = Time.diff start end
```

출력 결과는 다음과 같습니다.

```
Time.Days 0 <| Time.Hours 0 <| Time.Seconds 4023 <| 
Time.Millis 0 <| Time.Micros 0 <| Time.Nanos 0
```

위의 결과는 `Time.Span` 타입으로 반환되며, 기준 날짜의 단위로 두 날짜 간의 차이를 표시합니다. 위의 예제에서는 4023초 차이가 나는 것을 확인할 수 있습니다.

# 딥 다이브

두 날짜를 비교할 때 더 자세한 비교 방법이 필요할 수 있습니다. Elm에서는 `Time.Comparable` 모듈을 사용하여 두 날짜를 비교할 수 있습니다.

```Elm
import Time.Comparable
```

먼저 두 날짜를 같은 타입으로 변환해야 합니다. 위의 예제에서 사용했던 `Time.Posix`를 `Time.Comparable` 모듈에서 제공하는 `Time.Zone` 타입으로 변환할 수 있습니다.

```Elm
startZone : Time.Zone
startZone = Time.utc

endZone : Time.Zone
endZone = Time.zone startZone (-1 * 60 * 60 * 1000) -- 1시간 뒤
```

이제 `startZone`과 `endZone`을 사용하여 `Time.Comparable` 모듈에서 제공하는 `compare` 함수를 사용할 수 있습니다.

```Elm
compare : Time.Zone -> Time.Posix -> Time.Posix -> Time.Comparable.Order
compare startZone start end =
    Time.Comparable.compare startZone start end
```

출력 결과는 다음과 같습니다.

```
GT
```

위의 결과에서 `GT`는 두 날짜 중 뒤에 있는 `end` 변수가 더 크다는 것을 의미합니다. 이를 통해 두 날짜를 비교하는 더 많은 방법을 살펴볼 수 있습니다.

# 더 알아보기

위에서 소개한 내용외에도 Elm에서 날짜 비교에 사용할 수 있는 다른 함수들이 있습니다. `Time.Extra` 모듈에서는