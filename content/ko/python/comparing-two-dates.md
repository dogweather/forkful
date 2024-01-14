---
title:                "Python: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

날짜 비교는 데이터 처리에서 매우 중요한 기능입니다. 특히 세계 곳곳에서 사용되는 프로그래밍 언어인 파이썬에서는 날짜와 시간을 다루는 다양한 기능들이 제공되는데, 이 중 날짜를 비교하는 기능은 더욱 유용하게 사용될 수 있습니다.

## How To

비교하고 싶은 두 날짜를 파이썬의 datetime 객체로 만들고, 이를 통해 비교 연산을 수행할 수 있습니다. 아래 코드는 두 날짜가 같은지, 또는 첫 번째 날짜가 두 번째 날짜보다 이후인지를 비교하는 예시입니다.

```Python
from datetime import datetime

date1 = datetime(2021, 3, 15)
date2 = datetime(2021, 4, 5)

# 두 날짜가 같은지 비교
if date1 == date2:
    print("두 날짜가 같습니다.")

# 첫 번째 날짜가 두 번째 날짜보다 이후인지 비교
if date1 > date2:
    print("첫 번째 날짜가 두 번째 날짜보다 이후입니다.")
```

위 코드를 실행하면 "첫 번째 날짜가 두 번째 날짜보다 이후입니다."라는 출력 결과를 얻게 됩니다.

## Deep Dive

날짜를 비교할 때는 반드시 시간까지 함께 비교하는 것이 중요합니다. 만약 두 날짜가 같은 날짜지만 시간이 다를 경우, 같은 날짜로 간주되지 않고 비교 연산이 다른 결과를 도출할 수 있습니다. 따라서 날짜뿐만 아니라 시간도 잘 고려하여 비교하도록 주의해야 합니다.

또한, 날짜 비교에서 자주 사용되는 라이브러리 중 하나인 `dateutil`을 활용하면 더욱 간편한 날짜 비교 기능을 제공할 수 있습니다. `dateutil`을 사용하면 특정 날짜의 차이를 계산하거나, 날짜 사이의 거리를 구하는 등 다양한 기능들을 제공합니다.

## See Also

- 파이썬 공식 문서: https://docs.python.org/ko/3/library/datetime.html
- dateutil 공식 사이트: https://dateutil.readthedocs.io/en/stable/