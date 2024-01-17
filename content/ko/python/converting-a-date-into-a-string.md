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

## 무엇 & 왜?

날짜를 문자열로 변환한다는 것은 날짜를 텍스트 형태로 변경하는 것을 말합니다. 프로그래머들이 이를 하는 이유는 다양합니다. 예를 들면, 날짜를 데이터베이스에 저장하거나 출력 할 때, 문자열로 변환하는 것이 더 효율적이고 편리하기 때문입니다.

## 방법:

```Python
# datetime 모듈 임포트
import datetime

# 날짜를 문자열로 변환하기
today = datetime.datetime.today()
result = today.strftime("%Y년 %m월 %d일")
print(result)

# 출력 결과: 2020년 10월 20일
```

## 깊이있는 분석:

1. 역사적 배경: 날짜를 문자열로 변환하는 방법은 과거에 비해 더 효율적이고 간단해졌습니다. 예전에는 프로그래머가 직접 코드를 작성하여 날짜를 텍스트 형식으로 변환해야 했습니다.

2. 대안: 다른 언어에서도 날짜를 문자열로 변환하는 방법이 있지만, 파이썬의 경우 datetime 모듈을 사용하는 것이 가장 간단하고 효율적입니다.

3. 구현 세부 정보: 위 예제에서 사용된 "%Y년 %m월 %d일"은 날짜를 원하는 형식에 맞춰 출력하는 strftime 함수의 포맷 문자열입니다.

## 관련 자료:

- [datetime 모듈 문서](https://docs.python.org/3/library/datetime.html)
- [날짜와 시간 포맷 지정하기](https://strftime.org/)