---
title:                "Python: 날짜를 문자열로 변환하기"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜?
날짜를 문자열로 변환하는 일은 프로그래밍에서 자주 발생하게 됩니다. 예를 들어서, 데이터베이스에서 가져온 날짜를 웹사이트에서 보여줄 때 이를 문자열로 변환하여 보여주어야 합니다. 따라서 날짜를 문자열로 변환하는 것은 매우 중요한 일이며, 이를 효과적으로 할 수 있는 방법을 알고 있어야 합니다.

## 어떻게?
```Python
# datetime 모듈 불러오기
import datetime

# 현재 날짜 가져오기
today = datetime.date.today()

# 날짜를 문자열로 변환하고 출력
print(str(today))

# 포맷 지정하여 문자열로 변환하고 출력
print(today.strftime("%Y년 %m월 %d일"))
```

출력:
```
2021-01-01
2021년 01월 01일
```

## 더 깊게
날짜를 문자열로 변환하는 과정에서 발생할 수 있는 몇 가지 주의사항이 있습니다. 첫째, 날짜의 포맷을 지정할 때 주의해야 합니다. 특히, MM과 mm을 구분하는 것이 매우 중요합니다. MM은 월을 나타내며 01, 02, 03과 같이 0으로 시작하는 두자리 수로 표현됩니다. 반면에 mm은 분을 나타내며 00, 01, 02와 같이 0으로 시작하는 두자리 수로 표현됩니다.

또한, 날짜를 문자열로 변환하면서 로케일(local)과 타임존(timezone)에 대한 고려도 필요합니다. 로케일은 사용자의 언어, 국가 등을 나타내는 설정을 말하며, 타임존은 해당 지역의 표준시간대를 나타냅니다. 이를 고려하지 않고 날짜를 변환하면 사용자의 설정과 달리 다른 포맷으로 날짜를 표시하게 될 수 있습니다. 따라서 로케일과 타임존 설정을 잘 파악하고 사용해야 합니다.

## 더 알아보기
[KoreanDatetime 모듈](https://pypi.org/project/korean-lunar-calendar/)  
[strftime 포맷 문자열 참고 문서 (영문)](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)  
[날짜와 시간 처리에 대한 더 많은 정보 (영문)](https://www.programiz.com/python-programming/datetime)  

## 참고하기