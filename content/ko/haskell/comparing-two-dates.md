---
title:                "두 날짜 비교하기"
html_title:           "Haskell: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이고 왜 하는가?

두 날짜를 비교하느 것은 두 가지 날짜를 비교하여 어느 하나가 더 늦거나 빠른지를 알아내는 것입니다. 프로그래머들은 두 날짜를 비교하는 이유는 일반적으로 날짜 기반의 알고리즘 또는 데이터를 처리할 때 필요하기 때문입니다.

## 방법:

첫 번째 날짜가 더 늦은지 아닌지 확인하는 코드 예시:
```Haskell
firstDate `isEarlierThan` secondDate = firstDate < secondDate
```

두 날짜가 같은지 아닌지 확인하는 코드 예시:
```Haskell
firstDate `isSameAs` secondDate = firstDate == secondDate
```

## 깊이 파헤치기:

1. 역사적 배경: 날짜 비교 기능은 컴퓨터가 발명된 이래로 널리 사용되어온 기능 중 하나입니다. 초기 컴퓨터들은 숫자로 된 날짜를 문자로 변경한 다음 비교하였습니다. 하지만 현재의 컴퓨터들은 내부적으로 날짜를 표현하고 비교하는 기능을 갖추고 있습니다.
2. 대안: 날짜를 비교하지 않고도 요일이나 연도 등 날짜에 대한 정보가 필요한 경우, 라이브러리를 사용할 수 있습니다. 이를 통해 더 쉽게 날짜와 관련된 기능을 구현할 수 있습니다.
3. 구현 세부사항: Haskell의 기본 데이터 타입인 Data.Time은 날짜를 비교하는 데 필요한 함수들을 내장하고 있습니다. 따라서 별도의 라이브러리를 사용하지 않고도 날짜를 비교할 수 있습니다.

## 또한 보기:

- [Data.Time 라이브러리 문서](https://hackage.haskell.org/package/time/docs/Data-Time.html): Data.Time 라이브러리의 자세한 문서를 확인할 수 있습니다.