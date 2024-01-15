---
title:                "날짜를 문자열로 변환하기"
html_title:           "Python: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

누군가 날짜를 문자열로 변환하는 것에 참여하는 이유는 다양합니다. 가장 일반적인 이유는 날짜 데이터를 원하는 형식으로 표시하고자 할 때 입니다. 그리고 다른 경우에는 날짜 데이터를 활용하여 데이터 분석이나 시각화 작업 등을 수행하기 위해서 일 수도 있습니다.

## 방법

날짜를 문자열로 변환하는 방법에는 여러 가지가 있습니다. 가장 일반적인 방법은 `strftime()` 함수를 사용하는 것입니다. 이 함수는 매개변수로 날짜 형식을 지정해주어 해당 형식에 맞게 날짜를 문자열로 변환해줍니다.

```python
import datetime

today = datetime.date.today()
print(today.strftime("%Y-%m-%d"))
```

위의 코드는 현재 날짜를 `YYYY-MM-DD` 형식으로 출력하는 예시입니다. 다른 날짜 형식으로 출력하고 싶다면 `strftime()` 함수의 매개변수에 지정해주면 됩니다.

## 깊게 들어가보기

날짜를 문자열로 변환하는 방법을 깊게 들어가보면, `datetime` 모듈에서 제공하는 여러 함수와 메서드를 활용할 수 있습니다. 예를 들어, `strftime()` 함수 외에도 `strptime()` 함수를 이용하면 문자열을 날짜로 변환할 수 있습니다. 또한, `datetime` 객체의 `strftime()` 메서드를 사용하여 날짜를 원하는 형식으로 바로 출력할 수도 있습니다.

## 관련 자료

- [strftime() documentation](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
- [파이썬으로 날짜와 시간 다루기](https://wikidocs.net/23338)