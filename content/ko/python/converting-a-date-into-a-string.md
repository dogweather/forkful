---
title:                "날짜를 문자열로 변환하기"
html_title:           "Arduino: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은, 프로그래밍에서 주로 사용되는 기법 중 하나입니다. 이를테면 '2022-03-11'과 같이 표현된 문자열이나, '3월 11일, 2022년'같은 문자열로 날짜를 변환하는 것입니다. 이는 데이터를 사람이 이해하기 쉽게 표현하거나, 파일 이름 등에 날짜를 적용할 때 필요합니다.

## 적용 방법:

```python
from datetime import datetime

# 현재 날짜와 시간 가져오기
now = datetime.now()

# 날짜를 문자열로 변환하기: 'YYYY-MM-DD' 형식
date_str = now.strftime('%Y-%m-%d')
print(date_str)
```
위 코드를 실행하면 오늘의 날짜가 'YYYY-MM-DD' 형식의 문자열로 출력됩니다.

## 심화 학습

시간과 날짜를 문자열로 변환하는 기능은 Python의 datetime 모듈이 제공합니다. 이 모듈은 1990년대 초반 Python이 처음 개발될 당시부터 존재했습니다.

변환 외에도 'strftime' 함수는 다양한 날짜와 시간 포맷을 지원하므로, 필요에 따라 다른 형식을 사용할 수도 있습니다.

물론, 파이썬 외의 다른 프로그래밍 언어들도 이와 유사한 기능을 제공합니다. 단지 구현 방법이나 문법이 조금씩 다를 뿐입니다.

## 참고 자료

1. Python 공식 문서에서는 strftime 함수와 그 포맷에 대해 자세히 설명하고 있습니다: [https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior](https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior)
2. Python에 대한 더 깊은 이해를 위해, W3Schools Python Tutorial을 참고하세요: [https://www.w3schools.com/python/](https://www.w3schools.com/python/)