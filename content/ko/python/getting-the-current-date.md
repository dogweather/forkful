---
title:                "Python: 현재 날짜를 얻는 방법"
simple_title:         "현재 날짜를 얻는 방법"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

1. 제목: 현재 날짜를 얻는 이유

최신 프로그래밍 언어 중 하나인 파이썬에서 현재 날짜를 가져오는 방법을 배우기 전에 이것이 왜 유용한지 알아보겠습니다. 현재 날짜는 많은 애플리케이션에서 중요한 정보입니다. 예를 들어, 특정 이벤트가 발생한 날짜를 기록하거나 파일을 저장할 때 현재 날짜를 파일 이름으로 사용하는 등 다양한 용도로 사용될 수 있습니다.

이 기능은 정확성과 간편성을 제공하기 때문에 파이썬에서 제공하는 다양한 기능 중 하나입니다.

## 어떻게

이제 파이썬을 사용하여 현재 날짜를 가져오는 방법에 대해 알아보겠습니다.

```Python
# 현재 날짜를 가져오는 모듈을 불러옵니다
import datetime
# 현재 날짜를 가져와 변수에 저장합니다 
current_date = datetime.date.today()
# 현재 날짜를 출력합니다 
print(current_date)
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

현재 날짜를 가져오는 모듈을 불러옵니다
import datetime
# 현재 날짜를 가져와 변수에 저장합니다 
current_date = datetime.date.today()
# 현재 날짜를 출력합니다
print(current_date)

2021-07-30

위 코드에서 ```datetime``` 모듈을 사용하여 현재 날짜를 가져오고 이를 ```current_date``` 변수에 저장하였습니다. 그리고 이를 출력하여 현재 날짜를 확인할 수 있습니다.

시간 정보까지 필요한 경우, 다음과 같이 수정하여 사용할 수 있습니다.

```Python
# 현재 날짜와 시간을 출력하는 모듈을 불러옵니다
import datetime
# 현재 날짜와 시간을 변수에 저장합니다
current_datetime = datetime.datetime.now()
# 현재 날짜와 시간을 출력합니다
print(current_datetime)
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다.

2021-07-30 10:30:00.558455

## 더 깊은 알아보기

파이썬에서 날짜와 관련된 함수를 자세히 살펴보면 파이썬의 ```date``` 객체가 날짜와 관련된 작업을 수행하는 데 더 적합하다는 것을 알 수 있습니다. 이는 다양한 연산이 가능하며, 날짜 간의 계산, 파싱 및 출력에 효율적입니다.

파이썬에서 날짜와 관련된 더 많은 정보를 알고 싶다면 [파이썬 공식 문서](https://docs.python.org/3/library/datetime.html)를 참조하시기 바랍니다.

## 관련 링크

- [다른 프로그래밍 언어에서 현재 날짜를 가져오는 방법](https://www.w3schools.com/jsref/jsref_getdate.asp)
- [파이썬에서 현재 시간을 가져오는 다른 방법](https://www.programiz.com/python-programming/datetime/current-datetime)