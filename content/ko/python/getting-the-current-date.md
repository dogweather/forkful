---
title:                "Python: 현재 날짜 가져오기"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 것은 프로그래밍 작업에서 일상적인 작업입니다. 현재 날짜를 파악하면 날짜와 시간을 계산하거나 다양한 용도로 사용할 수 있습니다.

## 어떻게

파이썬의 "datetime" 모듈은 현재 날짜와 시간을 가져오는 기능을 제공합니다. 아래의 코드 블록에서는 현재 날짜와 시간을 출력하는 방법을 보여줍니다.

```python
from datetime import datetime

# 현재 시간 출력
current_time = datetime.now()
print("현재 시간:", current_time)

# 오늘의 날짜 출력
current_date = datetime.now().date()
print("오늘의 날짜:", current_date)
```

위의 코드에서 "datetime.now()" 메소드는 현재 날짜와 시간을 가져옵니다. 또한 ".date()" 메소드를 사용하여 날짜만 따로 출력할 수 있습니다. 이와 같은 기능을 활용하면 프로그램에서 필요한 현재 날짜와 시간을 쉽게 얻을 수 있습니다.

## 딥 다이브

"datetime" 모듈은 날짜와 시간을 다루는 다양한 기능을 제공합니다. 이러한 기능 중 일부를 소개해보겠습니다.

### 날짜 포맷 지정

"strftime()" 메소드를 사용하면 날짜와 시간의 출력 형식을 지정할 수 있습니다. 아래의 코드는 오늘의 날짜를 연월일 시분초로 포맷하여 출력하는 예시입니다.

```python
from datetime import datetime

current_date = datetime.now().strftime("%Y년 %m월 %d일 %H시 %M분 %S초")
print("오늘의 날짜:", current_date)
```

출력 결과: "2021년 10월 06일 15시 30분 20초"

### 날짜 계산

"timedelta" 객체를 사용하면 날짜와 시간에 대한 계산을 수행할 수 있습니다. 아래의 코드는 현재 날짜에서 30일 후의 날짜를 계산하는 예시입니다.

```python
from datetime import datetime, timedelta

current_date = datetime.now()
future_date = current_date + timedelta(days=30)
print("30일 후의 날짜:", future_date)
```

출력 결과: "30일 후의 날짜: 2021-11-05 15:30:20.465703"

## 관련 자료

- [Python datetime 모듈 공식 문서](https://docs.python.org/3.8/library/datetime.html)
- [datetime 모듈을 사용한 날짜/시간 처리 기능 예제](https://soooprmx.com/?p=4493)
- [파이썬으로 현재 날짜 확인하는 다양한 방법](https://umbum.dev/256)