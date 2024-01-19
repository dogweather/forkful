---
title:                "미래 또는 과거의 날짜 계산하기"
html_title:           "Python: 미래 또는 과거의 날짜 계산하기"
simple_title:         "미래 또는 과거의 날짜 계산하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

날짜를 미래 혹은 과거로 계산하는 것은, 특정 기간 후나 전의 날짜를 결정하는 행위입니다. 개발자들이 이를 수행하는 이유는 여러 가지: 이벤트 예약, 데이터 분석, 심지어 빌링 시스템에서도 이 작업을 수행합니다.

## 어떻게 해야 할까요?

Python의 datetime 모듈을 사용해 간단하게 해결할 수 있습니다.

```Python
import datetime

# 오늘 날짜
now = datetime.date.today()
print(now)

# 일주일 후
one_week_later = now + datetime.timedelta(weeks=1)
print(one_week_later)

# 하루 전
one_day_earlier = now - datetime.timedelta(days=1)
print(one_day_earlier)
```

## 딥 다이브

Python에서 날짜 계산의 역사는 어느 정도 길다. 가장 첫 번째 버전부터 현재 버전까지 이 기능이 있었습니다. 그러나 시간이 지나면서 Python 개발자들은 이 방법을 좀 더 간편하게 만들기 위해 다양한 라이브러리와 모듈을 개발하였습니다.

데이트타임 모듈 외에도 날짜 계산을 위한 다른 대안도 있습니다. 예를 들어, dateutil 라이브러리는 더 복잡한 날짜 계산을 할 수 있게 합니다.

날짜 계산은 'timedelta' 객체를 사용하여 수행됩니다. 이 'timedelta' 객체는 두 날짜나 시간 사이의 차이를 초 단위로 표현합니다.

## 참조하기

- Python 공식 문서: https://docs.python.org/ko/3/library/datetime.html
- dateutil 라이브러리: https://dateutil.readthedocs.io/en/stable/
- Python의 datetime 모듈 이해하기: https://dojang.io/mod/page/view.php?id=2463