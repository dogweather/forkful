---
title:                "Python: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜
날짜를 미래나 과거로 계산하는 데 참여하는 이유는 데이터 분석이나 일상적인 일정 관리에 유용하기 때문입니다.

## 어떻게
```Python
# 현재 날짜 가져오기
from datetime import datetime

# 미래 또는 과거의 날짜 계산
future_date = datetime.now() + timedelta(days=7)
past_date = datetime.now() - timedelta(days=7)

print(f"다가오는 날짜: {future_date}")
print(f"과거 날짜: {past_date}")
```

**출력:**
```
다가오는 날짜: 2020-09-25 22:47:13.932852
과거 날짜: 2020-09-11 22:47:13.932852
```

## 딥 다이브
날짜를 미래나 과거로 계산하는 데에는 여러 가지 방법이 있습니다. 일반적으로 `timedelta`를 사용하여 일정한 시간 간격을 정하여 날짜를 계산할 수 있습니다. 또는 `calendar` 모듈을 사용하여 원하는 날짜로 계산할 수도 있습니다. 이런 방법을 잘 익혀두면 데이터 분석이나 프로젝트 일정 관리에 유용하게 활용할 수 있습니다.

## 더 알아보기
- [Python Datetime 모듈 공식 문서](https://docs.python.org/3/library/datetime.html)
- [파이썬 데이트타임 모듈로 날짜 및 시간 캡처하기](https://stackabuse.com/how-to-capture-current-date-and-time-in-python/)
- [파이썬 날짜 계산하기 예제](https://www.programiz.com/python-programming/datetime)
- [파이썬 달력 모듈로 날짜 계산하기](https://www.datacamp.com/community/tutorials/python-date-time)
- [파이썬으로 미래 날짜 계산하기](https://stackoverflow.com/questions/546321/how-do-i-calculate-the-date-six-months-from-the-current-date-using-the-datetime)