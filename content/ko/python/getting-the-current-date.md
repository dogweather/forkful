---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
현재 날짜를 얻는 것은, 오늘 날짜를 출력하거나, 날짜 기반의 함수를 수행하는 코드를 작성하는 것을 말합니다. 프로그래머들이 이것을 하는 이유는 로그 파일에 타임스탬프를 새기거나, 데이터 추적 등의 목적으로 시간 관리를 할 수 있게 하기 위해서입니다. 

## 어떻게:
```Python
from datetime import date

# 현재 날짜 얻기
today = date.today()
print("오늘의 날짜는 ", today)
```
이 코드를 실행한다면, 출력 결과는 다음과 같습니다.
```
오늘의 날짜는  2022-03-26
```

## 딥 다이브
현재 날짜를 얻는 데는 여러 방법이 있습니다. `time` 모듈을 사용하는 방법도 있지만, 일반적으로 `datetime` 모듈을 사용하는 것이 더 명확하고 편리합니다. `datetime` 모듈은 Python 2.3 버전부터 추가되었습니다. 

대안으로, `strftime` 함수를 사용하여 날짜를 문자열 형태로 다룰 수도 있습니다. 예를 들어, `%Y-%m-%d` 형식을 사용하면 연도-월-일 형식으로 날짜를 출력할 수 있습니다.
```Python
from datetime import date

today = date.today()

# strftime 함수를 사용
d = today.strftime("%Y-%m-%d")
print("오늘의 날짜는 ", d)
```

## 추가 정보
다른 관련 자료를 탐색하려면 아래의 링크를 참고하십시오.
- Python 공식 문서: https://docs.python.org/ko/3/library/datetime.html
- Python 날짜와 시간 다루기 튜토리얼: https://dojang.io/mod/page/view.php?id=2463