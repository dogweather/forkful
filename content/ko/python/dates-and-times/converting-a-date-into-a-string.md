---
date: 2024-01-20 17:37:42.168689-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC83\uC740, \uD30C\uC774\uC36C\uC5D0\uC11C `datetime` \uAC1D\uCCB4\uB97C\
  \ \uD14D\uC2A4\uD2B8 \uD3EC\uB9F7\uC73C\uB85C \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\
  \uD569\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB0A0\uC9DC\uB97C \uD30C\uC77C \uC774\
  \uB984\uC5D0 \uC0AC\uC6A9\uD558\uAC70\uB098, \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uBCF4\
  \uAE30 \uC88B\uAC8C \uD45C\uD604\uD560 \uB54C \uD544\uC694\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.615510-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\uB294\
  \ \uAC83\uC740, \uD30C\uC774\uC36C\uC5D0\uC11C `datetime` \uAC1D\uCCB4\uB97C \uD14D\
  \uC2A4\uD2B8 \uD3EC\uB9F7\uC73C\uB85C \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\
  \uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## How to: (방법)
```python
from datetime import datetime

# 현재 날짜와 시간을 구함
now = datetime.now()

# 문자열로 변환: 기본 포맷
date_string_basic = now.strftime("%Y-%m-%d %H:%M:%S")
print(date_string_basic)  # 출력 예시: 2023-04-12 15:30:45

# 문자열로 변환: 사용자 정의 포맷
date_string_custom = now.strftime("%Y년 %m월 %d일")
print(date_string_custom)  # 출력 예시: 2023년 04월 12일
```

## Deep Dive (심화 탐구)
날짜를 문자열로 바꾸는 일은 초기 컴퓨터 시대부터 있었습니다. 초기에는 데이터 저장 공간이 귀하여, 날짜를 다루기 위한 표준 방식이 중요했습니다. `strftime` 함수는 C 언어의 `strftime`에서 유래되었으며, 파이썬에서도 이를 계승하여 사용합니다.

대안으로 `isoformat()`이 있습니다. 이는 ISO 8601 형식 (예: `2023-04-12T15:30:45.123456`)을 제공합니다. 간단한 경우 이 메소드를 사용하는 것이 유리할 수 있습니다.

날짜를 문자열로 변환할 때 문화적인 요소도 고려해야 할 때가 있습니다. 다른 국가는 다른 날짜 포맷을 사용하기 때문에, 파이썬의 `locale` 모듈로 지역화된 포맷을 설정할 수도 있습니다.

## See Also (추가 정보)
- Python 공식 문서의 `datetime` 모듈 설명: [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
- strftime()과 strptime()의 포맷 코드: [https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- ISO 8601, 날짜와 시간의 국제 표준: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
