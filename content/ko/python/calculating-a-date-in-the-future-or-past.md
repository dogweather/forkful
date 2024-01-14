---
title:    "Python: 미래나 과거의 날짜 계산하기"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

날짜를 미래나 과거로 계산하는 것이 유용한 이유는 많습니다. 예를 들어, 생일이나 기념일을 계산할 때, 미래의 일정을 계획할 때, 또는 미래의 일정을 확인할 때 등 다양한 상황에서 날짜를 계산해야 할 수 있습니다.

## 어떻게

다음과 같은 코드 블록을 사용하여 파이썬에서 날짜를 계산하는 방법을 알아보겠습니다.

```python
# 현재 날짜를 가져오기
import datetime

today = datetime.date.today()
print('오늘의 날짜:', today)

# 지정된 날짜 만들기
birthday = datetime.date(1990, 8, 25) # 생일을 년, 월, 일 순서로 입력
print('생일:', birthday)

# 날짜의 요일 확인하기
print('생일의 요일:', birthday.strftime('%A'))

# 미래의 날짜 계산하기
future_date = today + datetime.timedelta(days=30)
print('30일 후의 날짜:', future_date)

# 과거의 날짜 계산하기
past_date = today - datetime.timedelta(days=365)
print('일년 전의 날짜:', past_date)
```

위의 코드를 실행하면 다음과 같은 결과가 출력됩니다.

```
오늘의 날짜: 2021-09-10
생일: 1990-08-25
생일의 요일: Saturday
30일 후의 날짜: 2021-10-10
일년 전의 날짜: 2020-09-10
```

## 깊이 파헤치기

파이썬에서는 날짜와 관련된 다양한 기능을 제공합니다. `datetime` 라이브러리를 사용하면 현재 날짜를 가져오는 것은 물론, 지정된 날짜를 생성하고 리눅스 시간표현식과 호환되는 날짜로 변환할 수도 있습니다. 또한 `timedelta`를 사용하면 미래나 과거의 날짜를 손쉽게 계산할 수 있습니다.

파이썬에서 날짜와 관련된 더 많은 기능을 알아보고 싶다면 공식 문서를 참고해 보세요. 더 많은 예제 코드와 자세한 설명을 볼 수 있습니다.

## 이어보기

- [파이썬 공식 문서: 날짜 및 시간](https://docs.python.org/3/library/datetime.html)
- [The Python datetime Module: A Beginner's Guide](https://realpython.com/python-datetime/)
- [Python Datetime Module: Manipulate Dates and Times Effortlessly](https://dbader.org/blog/python-datetime-timezone)
- [Python Tips: Working with Dates and Times](https://www.programiz.com/python-programming/datetime)