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

## 무엇 & 왜?

두 날짜를 비교한다는 것은 일반적으로 두 날짜 중 보다 빠른 혹은 늦은 날짜를 찾는 작업을 말합니다. 이는 프로그래머들이 코드를 작성하고 데이터를 정렬하거나 분석하기 위해서 자주 사용되는 작업입니다.

## 하는 방법:

```Python
# 두 날짜 비교
date1 = "2021-10-01"
date2 = "2021-09-01"

if date1 > date2:
    print("date1이 더 늦은 날짜입니다.")
elif date1 < date2:
    print("date2가 더 늦은 날짜입니다.")
else:
    print("두 날짜는 같은 날짜입니다.")
```

```
Output:
date1이 더 늦은 날짜입니다.
```

## 더 깊이 파보기:
두 날짜를 비교하는 것은 오래전부터 사용되어온 개념입니다. 오늘날에는 다양한 프로그래밍 언어와 라이브러리에서 이를 지원하고 있으므로 쉽게 구현할 수 있습니다. 다른 방법으로는 날짜를 숫자로 변환하여 비교하는 것이 있습니다. 이는 특정 날짜를 기준으로 해서 그보다 늦은 날짜는 양수, 그보다 빠른 날짜는 음수로 나타내는 방식입니다.

## 더 알아보기:
- [Python date comparison](https://www.tutorialspoint.com/python/python_date_time.htm)
- [날짜 비교 관련 라이브러리](https://pypi.org/project/dateparser/)
- [날짜와 시간을 다루는 팁과 트릭](https://www.digitalocean.com/community/tutorials/how-to-work-with-date-and-time-in-python-3)

## 관련 자료:
- [Python 공식 문서](https://docs.python.org/3/library/datetime.html#datetime.date)