---
title:                "미래나 과거의 날짜 계산하기"
date:                  2024-01-20T17:31:49.197915-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
미래 또는 과거의 날짜 계산이 무엇인지 아세요? 간단히 말해, 특정 날짜에서 몇 일을 더하거나 빼서 새로운 날짜를 구하는 거예요. 이건 예약시스템, 마감일 추적, 기념일 알림 같은 기능에서 유용하죠.

## How to: (어떻게:)
```Python
from datetime import datetime, timedelta

# 오늘 날짜 구하기
today = datetime.now()

# 10일 후 날짜 계산
future_date = today + timedelta(days=10)
print("10일 후:", future_date.strftime("%Y-%m-%d"))

# 5일 전 날짜 계산
past_date = today - timedelta(days=5)
print("5일 전:", past_date.strftime("%Y-%m-%d"))
```

### Sample Output
```
10일 후: 2023-04-20
5일 전: 2023-04-05
```

## Deep Dive (심층 분석)
과거에는 `timedelta` 같은 편리한 도구가 없었어요. 개발자들은 원시적인 방법으로 날짜를 계산하곤 했죠. 파이썬은 이 과정을 `datetime` 모듈로 매끄럽게 해 줍니다. 이 모듈은 또한 시간대 관련 처리를 위해 `pytz` 같은 타 라이브러리와도 잘 작동해요. 날짜 계산은 내부적으로 날짜를 유닉스 타임스탬프(1970년 1월 1일부터 계산된 초)로 변환하고 연산 후 다시 날짜로 변환하는 방식으로 이루어집니다.

## See Also (참고 자료)
- datetime 모듈 공식 문서: https://docs.python.org/3/library/datetime.html
- pytz 라이브러리: http://pytz.sourceforge.net/
- timedelta 공식 문서: https://docs.python.org/3/library/datetime.html#timedelta-objects