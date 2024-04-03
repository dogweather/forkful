---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:09.658496-07:00
description: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\
  \uB2E4\uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB85C \uB41C \uB0A0\uC9DC\uC640 \uC2DC\
  \uAC04 \uC815\uBCF4\uB97C datetime \uAC1D\uCCB4 \uB610\uB294 \uB3D9\uB4F1\uD55C\
  \ \uAD6C\uC870\uD654\uB41C \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\
  \uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB294 \uC8FC\uB85C \uB0A0\uC9DC \uC0B0\uC220\
  , \uBE44\uAD50 \uBC0F \uD3EC\uB9F7\uD305 \uC791\uC5C5\uC744 \uC5B8\uC5B4 \uBC0F\
  \ \uC9C0\uC5ED\uC5D0 \uAD6C\uC560\uBC1B\uC9C0 \uC54A\uB294 \uBC29\uC2DD\uC73C\uB85C\
  \ \uAC00\uB2A5\uD558\uAC8C \uD558\uAE30 \uC704\uD574 \uC218\uD589\uB429\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\u2026"
lastmod: '2024-03-13T22:44:54.612542-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC\uB97C \uD30C\uC2F1\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uD14D\uC2A4\uD2B8\uB85C \uB41C \uB0A0\uC9DC\uC640 \uC2DC\uAC04\
  \ \uC815\uBCF4\uB97C datetime \uAC1D\uCCB4 \uB610\uB294 \uB3D9\uB4F1\uD55C \uAD6C\
  \uC870\uD654\uB41C \uD615\uC2DD\uC73C\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\uC744\
  \ \uB9D0\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

## 방법:
Python의 표준 라이브러리는 이 목적을 위한 `strptime` 메소드를 포함하는 `datetime` 모듈을 제공합니다. 이 메소드는 날짜 문자열과 입력 문자열의 패턴을 지정하는 형식 지시문이라는 두 가지 인수가 필요합니다.

```python
from datetime import datetime

# 예시 문자열
date_string = "2023-04-01 14:30:00"
# 문자열을 datetime 객체로 파싱
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# 출력: 2023-04-01 14:30:00
```

특히 여러 형식이나 로케일을 다룰 때 더 미묘한 날짜 파싱이 필요한 경우, `dateutil`과 같은 타사 라이브러리가 매우 유용할 수 있습니다. 이는 거의 모든 문자열 형식의 날짜를 파싱할 수 있는 파서 모듈을 제공합니다.

```python
from dateutil import parser

# 예시 문자열들
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# dateutil의 파서 사용
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# 출력: 2023-04-01 14:30:00
print(parsed_date2)
# 출력: 2023-04-01 14:30:00
```

`dateutil`은 명시적 형식 문자열 없이도 대부분의 날짜 형식을 처리할 수 있어, 다양한 날짜 표현을 다루는 애플리케이션에 다재다능한 선택이 됩니다.
