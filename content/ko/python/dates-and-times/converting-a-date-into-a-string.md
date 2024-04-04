---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\uBC29\uBC95: Python\uC740 \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C\
  \ \uBCC0\uD658\uD558\uB294 \uAC83\uC744 \uC27D\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4. [\uB0A0\
  \uC9DC](https://docs.python.org/3/library/datetime.html#date-objects) \uAC1D\uCCB4\
  \uC5D0\uC11C \uC0AC\uC6A9 \uAC00\uB2A5\uD55C\u2026"
lastmod: '2024-04-04T02:02:59.563460-06:00'
model: gpt-4-0125-preview
summary: "Python\uC740 \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\
  \uB294 \uAC83\uC744 \uC27D\uAC8C \uB9CC\uB4ED\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## 방법:
Python은 날짜를 문자열로 변환하는 것을 쉽게 만듭니다. [날짜](https://docs.python.org/3/library/datetime.html#date-objects) 객체에서 사용 가능한 [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) 메서드를 사용하세요. 방법은 다음과 같습니다:

```Python
from datetime import datetime

# 현재 날짜와 시간을 얻기
now = datetime.now()

# 월 일, 연도의 형식으로 문자열로 변환하기
date_string = now.strftime("%B %d, %Y")
print(date_string)  # 출력: 2023년 3월 29일 (혹은 현재 날짜)

# 형식: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # 출력: 2023-03-29 (혹은 현재 날짜)
```


### 내가 하는 방법

다음은 타임존 정보가 포함된 [ISO 8601](https://www.w3.org/QA/Tips/iso-date) 형식 날짜를 얻는 방법입니다:

```python
def datestamp() -> str:
    """ 
    타임존이 포함된 현재 날짜와 시간을 ISO 형식으로.
    """
    return datetime.now().astimezone().isoformat()
```

#### 예시 출력:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## 깊이 들여다보기
역사적으로, 날짜를 사람이 읽을 수 있는 형식으로 표현할 필요성 때문에, 날짜-문자열 변환은 프로그래밍에서 핵심 주제가 되어 왔습니다.

`strftime`에 대한 대안으로는 ISO 8601 형식에 대한 `isoformat` 메서드나, 더 유연한 파싱 및 형식 지정 옵션을 제공하는 `arrow` 및 `dateutil`과 같은 타사 라이브러리를 사용하는 것이 있습니다.

구현 관점에서 보면, `strftime`은 "string format time"의 약자이며 C 프로그래밍에서 그 기원을 찾을 수 있습니다. Python의 `strftime`은 연도를 위한 `%Y`, 월을 위한 `%m`과 같은 형식 코드를 해석하며, 거의 무한한 사용자 정의 가능성을 허용합니다.

## 참고
Python의 날짜 및 시간 함수에 대해 더 깊이 들어가고 싶다면:
- Python 공식 `datetime` 문서: https://docs.python.org/3/library/datetime.html
- `strftime` 지시어의 포괄적인 목록에 관심이 있다면: https://strftime.org/
- 타사 날짜/시간 라이브러리 탐색하기:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
