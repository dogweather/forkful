---
title:                "두 날짜 비교하기"
html_title:           "Python: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
두 날짜를 비교하는 데에는 왜 관심을 가질까요?
두 날짜를 비교하면서 우리는 두 날짜가 같은지, 어느 쪽이 앞선 날짜인지 혹은 두 날짜 사이의 차이가 얼마나 있는지 등을 알 수 있습니다. 이것은 우리 일상에서 날짜를 다룰 때 매우 유용한 기능이 됩니다.

## 어떻게
Python에서는 날짜를 비교하기 위해 datetime 모듈을 사용합니다. 이번에는 datetime 모듈을 활용하여 두 날짜를 비교하는 방법을 알아보겠습니다. 아래의 코드를 따라해보세요.

```Python
from datetime import date

# 비교하고 싶은 두 날짜를 변수로 정의합니다.
first_date = date(2020, 5, 20)
second_date = date(2020, 6, 15)

# 두 날짜가 같은지 비교합니다.
if first_date == second_date:
    print("두 날짜는 같습니다.")
else:
    print("두 날짜는 다릅니다.")

# 두 날짜 중 어느 날짜가 앞선 날짜인지 비교합니다.
if first_date < second_date:
    print("first_date가 second_date보다 앞섭니다.")
else:
    print("first_date가 second_date보다 뒤에 있습니다.")

# 두 날짜 사이의 차이를 계산합니다.
difference = second_date - first_date
print("두 날짜 사이의 차이는 {}일입니다.".format(difference.days))
```
출력 결과:
```
두 날짜는 다릅니다.
first_date가 second_date보다 앞섭니다.
두 날짜 사이의 차이는 26일입니다.
```

## 딥 다이브
비교하는 두 날짜의 형식이 다를 경우, Python에서는 datetime 모듈의 `strptime()` 함수를 활용하여 문자열로 된 날짜를 datetime 객체로 변환할 수 있습니다. 또한, 비교할 때 두 날짜의 시간도 고려할 수 있습니다. 자세한 내용은 [문서](https://docs.python.org/3/library/datetime.html)를 참고하세요.

## 확인해보세요
[파이썬에서 날짜 다루는 법](https://www.datacamp.com/community/tutorials/python-datetime-tutorial) - datetime 모듈의 다양한 기능을 소개하는 자습서입니다.