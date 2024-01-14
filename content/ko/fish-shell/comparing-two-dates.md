---
title:                "Fish Shell: 두 날짜를 비교하는 방법"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
날짜를 비교하는 것이 왜 중요한지 궁금하다면 이 글을 읽어보세요. Fish Shell을 이용하여 날짜를 비교하는 방법을 알려드리겠습니다. 또한 어떤 상황에서 날짜를 비교하는 것이 유용한지에 대해서도 알아보겠습니다.

## 어떻게
날짜를 비교하기 위해선 Fish Shell의 `date +%s` 명령어가 필요합니다. 이 명령어는 현재 날짜를 UNIX 시간으로 반환해줍니다. 예를 들어서 오늘의 날짜를 비교하려면 다음과 같이 입력해주세요:

```Fish Shell
set today (date +%s)
echo $today
```

출력값은 현재 시간을 나타내는 숫자일 것입니다. 이제 비교할 날짜를 입력해서 두 날짜를 비교할 수 있습니다. 이 경우에는 UNIX 시간으로 변환하기 위해 `string toarr -i [your_date]`를 사용할 수 있습니다. 예를 들어서 5월 15일 2020년의 UNIX 시간을 구하려면 다음과 같이 하면 됩니다:

```Fish Shell
set date1 1589529600
set date2 (string toarr -i 2020-05-15)
echo $date2
```

이제 `$date1`과 `$date2`를 비교할 수 있습니다. 예를 들어서 두 날짜가 동일하다면 `echo "Dates are equal!"`를 출력하게 할 수 있습니다.

## 깊이 파고들기
날짜를 비교하기 위해선 UNIX 시간을 사용해야하는 이유가 무엇일까요? UNIX 시간은 1970년 1월 1일부터 현재까지의 초를 나타내는 숫자 값입니다. 따라서 이를 이용하면 날짜를 쉽게 비교할 수 있습니다. 또한 Fish Shell은 삼항 연산자를 지원하기 때문에 UNIX 시간을 비교할 때 유용하게 사용할 수 있습니다.

## 관련글보기
- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [UNIX 시간에 대한 더 많은 정보](https://ko.wikipedia.org/wiki/UNIX_%EC%8B%9C%EA%B0%84)