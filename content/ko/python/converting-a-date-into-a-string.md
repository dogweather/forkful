---
title:    "Python: 날짜를 문자열로 바꾸기"
keywords: ["Python"]
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 것이 유용한 이유

## 이렇게 하는 방법

```Python
# import the datetime library
import datetime

# create a datetime object with today's date
now = datetime.datetime.now()

# use strftime() method to convert date into string
date_string = now.strftime("%Y년 %m월 %d일")

# print the output
print("오늘의 날짜는 " + date_string + "입니다.")

```

**출력**

`오늘의 날짜는 2021년 07월 26일입니다.`

## 깊이 알아보기

날짜를 문자열로 변환하는 것은 프로그래밍에서 매우 일반적인 작업 중 하나입니다. 날짜를 문자열로 변환하려면 `strftime()` 메소드를 사용하는데, 이 메소드는 DateTime 객체에 저장된 날짜를 사용자가 지정한 형식에 맞게 문자열로 변환해줍니다. 이를 사용하면 날짜를 보기 좋고 가독성이 좋은 형식으로 표시할 수 있습니다. 또한 날짜와 관련된 계산이나 비교 등 다양한 작업에 유용하게 사용할 수 있습니다.

## 또 다른 정보

### 날짜 및 시간 포맷 문서 (Python 공식 문서)

https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes

### Python datetime 라이브러리 (TutorialsPoint)

https://www.tutorialspoint.com/python/python_date_time.htm

### 날짜와 관련된 기능 (Real Python)

https://realpython.com/date-and-time-data-types-and-tools/