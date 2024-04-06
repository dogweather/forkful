---
date: 2024-01-20 17:33:44.860756-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uB0A0\uC9DC \uBE44\
  \uAD50\uB294 `datetime` \uBAA8\uB4C8\uB85C \uD0AC\uB7EC \uAE30\uB2A5\uC744 \uBC1C\
  \uD718\uD569\uB2C8\uB2E4. 2003\uB144\uC5D0 \uCC98\uC74C \uB098\uC628 \uC774\uD6C4\
  , Python\uC740 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uC744 \uB2E4\uB8E8\uB294 \uAC15\uB825\
  \uD55C \uD45C\uC900 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC81C\uACF5\uD569\uB2C8\
  \uB2E4. \uB300\uC548\uC73C\uB85C `dateutil` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00\
  \ \uC788\uC9C0\uB9CC, \uB300\uBD80\uBD84 \uD45C\uC900\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:09.101002-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) \uB0A0\uC9DC \uBE44\uAD50\uB294\
  \ `datetime` \uBAA8\uB4C8\uB85C \uD0AC\uB7EC \uAE30\uB2A5\uC744 \uBC1C\uD718\uD569\
  \uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to: (어떻게 하나요?)
```Python
from datetime import datetime

# 날짜 생성
date1 = datetime(2023, 4, 1)
date2 = datetime(2023, 4, 15)

# 날짜 비교
is_before = date1 < date2
days_difference = (date2 - date1).days

# 출력
print(f"Is date1 before date2? {is_before}")
print(f"The difference in days is: {days_difference}")

# 샘플 출력
# Is date1 before date2? True
# The difference in days is: 14
```

## Deep Dive (심도 있는 탐구)
날짜 비교는 `datetime` 모듈로 킬러 기능을 발휘합니다. 2003년에 처음 나온 이후, Python은 날짜와 시간을 다루는 강력한 표준 라이브러리를 제공합니다. 대안으로 `dateutil` 라이브러리가 있지만, 대부분 표준 `datetime`이면 충분해요. 비교하는 것은 내부적으로 날짜와 시간을 POSIX 타임스탬프로 변환해서 수행합니다. 이는 1970년 1월 1일 0시 0분 0초로부터의 초 수를 의미하죠.

## See Also (더 보기)
- Python 공식 문서에서 `datetime` 모듈: https://docs.python.org/3/library/datetime.html
- Python 날짜와 시간 다루기, Real Python 가이드: https://realpython.com/python-datetime/
- `dateutil` 라이브러리 문서: https://dateutil.readthedocs.io/en/stable/
