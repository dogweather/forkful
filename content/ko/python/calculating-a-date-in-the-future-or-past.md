---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Python: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 계산하는가?

지금으로부터 미래나 과거에 있는 특정 날짜를 계산하는 것은 프로그래머들이 일반적으로 하는 작업 중 하나입니다. 우리는 종종 특정 날짜를 알고 싶거나, 날짜 계산을 사용하여 특정 작업을 자동화하고 싶어할 때 이를 수행합니다.

## 하는 방법:

```python
# 오늘 날짜로부터 7일 후의 날짜 계산
from datetime import date, timedelta

today = date.today()
future_date = today + timedelta(days=7)

print(future_date)
# 출력: 오늘로부터 7일 후의 날짜 (YYYY-MM-DD 형식)

# 과거 날짜 계산도 가능합니다.
past_date = today - timedelta(days=30)
print(past_date)
# 출력: 오늘로부터 30일 전의 날짜 (YYYY-MM-DD 형식)
```

이것은 우리가 미래나 과거에 있는 특정 날짜를 계산하는 가장 간단한 방법입니다. 더 복잡한 계산을 위해서는 `timedelta` 객체의 다른 속성(주, 시간, 초 등)을 사용할 수 있습니다.

## 깊이 파헤치기:

이 날짜 계산 기술은 지금까지 꼭 날짜를 사용하는 모든 컴퓨터 시스템에서 사용되어왔습니다. 그러나 더 최근에 우리는 이 작업을 더 효율적으로 처리하기 위해 라이브러리들을 개발했습니다. 예를 들어, `dateutil.relativedelta`와 같은 라이브러리는 `timedelta`와 유사한 방식으로 날짜를 계산하지만 더 많은 유연성을 제공합니다. 또한, `calendar` 모듈을 사용하여 특정 날짜의 요일을 확인하거나, 윤년 여부를 알 수도 있습니다.

## 관련 링크:

- Python 공식 문서: [`datetime` 모듈](https://docs.python.org/3/library/datetime.html)
- `dateutil` 라이브러리 문서: [Python `dateutil` 문서](https://dateutil.readthedocs.io/en/stable/)
- `calendar` 모듈 문서: [Python `calendar` 모듈 문서](https://docs.python.org/3/library/calendar.html)