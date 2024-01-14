---
title:                "Python: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
날짜를 문자열로 변환하는 것에 대해 이야기하기 전에, 이 기술이 이해하고 활용하기 위해 왜 필요한지 알아봅시다.

날짜를 이해하고 사용하는 것은 많은 프로그래밍 작업에서 중요합니다. 예를 들어, 정기적인 보고서를 만들거나 이메일을 보낼 때, 날짜는 매우 중요한 정보입니다. 하지만 컴퓨터는 날짜를 일반적인 숫자로 저장하기 때문에, 이를 사용하기 위해서는 문자열로 변환해야 합니다. 이번 포스트에서는 Python에서 날짜를 문자열로 변환하는 방법을 알아보겠습니다.

## 어떻게
다음 예제 코드를 통해 Python에서 날짜를 문자열로 변환하는 방법을 알아봅시다.

```Python
# datetime 라이브러리의 date 모듈을 import 합니다
from datetime import date

# 오늘 날짜를 가져옵니다
today = date.today()

# strftime 메소드를 사용하여 날짜를 원하는 형식의 문자열로 변환합니다
date_string = today.strftime("%Y년 %m월 %d일")

# 변환된 문자열을 출력합니다
print("오늘은" + date_string + "입니다.")
```

위 코드의 출력은 다음과 같을 것입니다:

> 오늘은 2021년 03월 25일입니다.

날짜를 문자열로 변환하기 위해 `strftime()` 메소드를 사용하였습니다. 이 메소드는 두 가지 매개변수를 받습니다. 첫 번째 매개변수는 원하는 날짜 형식을 지정하는 포맷 문자열이며, 두 번째 매개변수는 날짜를 나타내는 변수입니다.

포맷 문자열에서 사용할 수 있는 자리 표시자들은 다양한데, 여기서는 `"%Y"`는 연도를 나타내고, `"%m"`은 월을 나타내며, `"%d"`는 일을 나타냅니다. 더 많은 자리 표시자들과 그 의미는 [Python 공식 문서](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)에서 확인할 수 있습니다.

## 딥 다이브
지금까지 날짜를 문자열로 변환하는 간단한 예제를 살펴보았습니다. 하지만 실제로는 아주 많은 방법으로 날짜를 변환할 수 있습니다. 예를 들어, 날짜와 시간을 모두 표시하는 문자열, 요일을 표시하는 문자열 등의 변환 방법도 있습니다.

또한, 날짜를 문자열로 변환하는 것 외에도 반대로 문자열을 날짜로 변환할 수도 있습니다. 이를 위해선 `strptime()` 메소드를 사용하면 됩니다. 자세한 내용은 [다음 포스트](https://blog.naver.com/programming-python/222264419781)에서 다룰 예정입니다.

이미 존재하는 문자열을 포맷 문자열로 사용하거나, 다른 프로그래밍 언어에서 날짜 포맷을 Python에 적용하는 방법 등도 더 알아보실 수 있습니다.

## 관련 자료
- [Python 공식 문서 - 날짜와 시간 형식](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)
- [날짜와 시간 포맷 표에 대한 더 많은 정보](https://strftime.org/)
- [Python datetime 라이브러리에 대한 자세한 설명](