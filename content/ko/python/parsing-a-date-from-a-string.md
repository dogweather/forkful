---
title:                "문자열에서 날짜 분석하기"
html_title:           "Gleam: 문자열에서 날짜 분석하기"
simple_title:         "문자열에서 날짜 분석하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇과 왜?: 

문자열에서 날짜를 파싱한다는 것은 문자열을 날짜와 시간 데이터로 변환하는 것입니다. 프로그래머들이 이것을 하는 이유는 날짜와 시간을 실제로 계산하고 비교하려는 경우, 이를 수행하기 위해 문자열 형식에서 날짜 형식으로 변환해야하기 때문입니다.

## 어떻게 해야 합니까?:

Python에서 문자열로부터 날짜를 파싱하려면 datetime 모듈을 사용해야 합니다.
```Python
from datetime import datetime

date_string = "2022-05-15"
date_object = datetime.strptime(date_string, "%Y-%m-%d")

print(date_object)
```
위 코드를 실행하면 결과는 아래와 같습니다.
```
2022-05-15 00:00:00
```
## 깊이 파헤치기:

문자열에서 날짜를 파싱하는 것은 프로그래밍의 공통적인 작업 중 하나로, 이는 프로그램이 날짜 및 시간 정보를 전달하는 데 사용되는 표준적인 방법입니다. 파이썬 외의 많은 언어, 예를 들어 자바, 자바스크립트, C# 등도 이 기능을 지원합니다. 문자열에서 날짜 파싱은 비교적 간단하나, 다양한 형식의 날짜와 시간 문자열을 파싱해야 하는 경우 복잡성이 늘어납니다. 커스텀 날짜/시간 형식을 만들어 파싱하는 방법으로, strftime() 및 strptime() 함수를 사용합니다.

## 관련 링크:

1. Python의 공식 datetime 문서: https://docs.python.org/3/library/datetime.html
2. 날짜와 시간에 대한 Python Cookbook: https://pymotw.com/3/datetime/index.html
3. strftime() 및 strptime() 함수에 대한 설명: https://www.programiz.com/python-programming/datetime/strftime.