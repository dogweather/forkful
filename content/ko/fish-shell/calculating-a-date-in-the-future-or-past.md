---
title:                "Fish Shell: 미래나 과거의 날짜 계산하기"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

일자를 미래나 과거로 계산하는 일에 참여하는 이유는 전통적인 달력으로는 원하는 날짜를 찾기 어렵기 때문입니다.

# 하는 법

일자를 계산하는 가장 쉬운 방법은 과거나 미래 중 한 가지 시점을 정해놓고 거기서 얼마만큼 떨어져 있는지 계산하는 것입니다. 이를 위해서는 Fish Shell에 있는 `date` 함수를 활용합니다.

```
# 오늘날짜 계산하기
set today (date '+%Y-%m-%d')

# 1년 후 날짜 계산하기
echo (date -d "+1 year" '+%Y-%m-%d')

# 3달 전 날짜 계산하기
echo (date -d "-3 months" '+%Y-%m-%d')
```

위와 같이 `date` 함수에 특정 시간을 설정해주면 해당 시점으로부터 얼마나 떨어져 있는지를 계산할 수 있습니다. `date` 함수에서 사용할 수 있는 옵션들은 다양하니 원하는 계산 방식에 따라 적절한 옵션을 사용하면 됩니다.

## 깊이 파고들기

일자를 계산하는 것은 너무나도 단순한 작업처럼 보이지만, 실제로는 많은 복잡성을 가지고 있습니다. 우리가 사용하는 달력은 기원전 45년 이후에 Gregorian 달력이라는 것이 도입되었지만, 이전에는 다양한 달력이 사용되어 왔습니다. 때문에 과거로 갈수록 달력의 차이가 더욱 커지게 됩니다. 또한 서머타임과 윤년과 같은 특수한 상황들을 고려해야 하는데, 이러한 것들을 고려하지 않고 계산을 한다면 원하지 않는 결과가 나올 수 있습니다.

# 관련 정보

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [GNU `date` 함수 문서](https://www.man7.org/linux/man-pages/man1/date.1.html)
- [달력의 역사 및 종류](https://www.thoughtco.com/history-of-calendars-1991903)