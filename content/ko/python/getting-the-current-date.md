---
title:                "현재 날짜 가져오기"
html_title:           "Python: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

최신 날짜를 얻는 것이란 무엇일까요? 프로그래머들이 왜 이 작업을 하는 걸까요?

현재 날짜를 얻는 것은 현재 시간을 알기 위해서입니다. 프로그래머들은 이를 사용하여 소프트웨어의 특정 기능과 일정한 기간을 연결할 수 있습니다. 또한 데이터를 정렬하거나 현재 시간의 구조를 이용하여 특정 작업을 수행하는 데에도 사용됩니다.

## 사용 방법:

### 예제 1: 현재 날짜 및 시간 출력하기

```python
import datetime

current_time = datetime.datetime.now()
print("현재 날짜 및 시간: ", current_time)
```

출력 결과:
현재 날짜 및 시간: 2021-05-24 20:30:00.000000

### 예제 2: 현재 날짜만 출력하기

```python
import datetime

current_date = datetime.date.today()
print("오늘의 날짜: ", current_date)
```

출력 결과:
오늘의 날짜: 2021-05-24

## 더 들여다보기:

### 역사적 배경:

현재 날짜를 얻는 방법은 프로그래밍 언어와 운영 체제에 따라 다를 수 있지만, 대부분의 언어와 운영 체제에서 기본적인 함수 또는 라이브러리를 통해 현재 날짜를 얻을 수 있습니다. 초기에는 현재 날짜를 얻는 것이 복잡한 작업이었지만, 현재는 대부분의 프로그래밍 언어에서 쉽게 구현할 수 있습니다.

### 대안:

현재 날짜를 얻기 위해서는 파이썬의 datetime 라이브러리를 사용하는 것이 가장 일반적인 방법입니다. 다른 대안으로는 time 모듈이 있지만, datetime보다는 낮은 수준의 기능만 제공합니다. 또한 서드파티 라이브러리 중에는 더 다양한 날짜 및 시간 관련 기능을 제공하는 것들도 있습니다.

## 관련 자료:

- [Python datetime 모듈](https://docs.python.org/3/library/datetime.html)
- [time 모듈](https://docs.python.org/3/library/time.html)
- [popular third-party date and time libraries for Python](https://realpython.com/python-datetime/#third-party-date-time-libraries)