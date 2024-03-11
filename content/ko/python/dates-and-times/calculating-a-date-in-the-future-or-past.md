---
date: 2024-01-20 17:31:49.197915-07:00
description: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\
  \uC774 \uBB34\uC5C7\uC778\uC9C0 \uC544\uC138\uC694? \uAC04\uB2E8\uD788 \uB9D0\uD574\
  , \uD2B9\uC815 \uB0A0\uC9DC\uC5D0\uC11C \uBA87 \uC77C\uC744 \uB354\uD558\uAC70\uB098\
  \ \uBE7C\uC11C \uC0C8\uB85C\uC6B4 \uB0A0\uC9DC\uB97C \uAD6C\uD558\uB294 \uAC70\uC608\
  \uC694. \uC774\uAC74 \uC608\uC57D\uC2DC\uC2A4\uD15C, \uB9C8\uAC10\uC77C \uCD94\uC801\
  , \uAE30\uB150\uC77C \uC54C\uB9BC \uAC19\uC740 \uAE30\uB2A5\uC5D0\uC11C \uC720\uC6A9\
  \uD558\uC8E0."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:28.533401-06:00'
model: gpt-4-1106-preview
summary: "\uBBF8\uB798 \uB610\uB294 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uC774\
  \ \uBB34\uC5C7\uC778\uC9C0 \uC544\uC138\uC694? \uAC04\uB2E8\uD788 \uB9D0\uD574,\
  \ \uD2B9\uC815 \uB0A0\uC9DC\uC5D0\uC11C \uBA87 \uC77C\uC744 \uB354\uD558\uAC70\uB098\
  \ \uBE7C\uC11C \uC0C8\uB85C\uC6B4 \uB0A0\uC9DC\uB97C \uAD6C\uD558\uB294 \uAC70\uC608\
  \uC694. \uC774\uAC74 \uC608\uC57D\uC2DC\uC2A4\uD15C, \uB9C8\uAC10\uC77C \uCD94\uC801\
  , \uAE30\uB150\uC77C \uC54C\uB9BC \uAC19\uC740 \uAE30\uB2A5\uC5D0\uC11C \uC720\uC6A9\
  \uD558\uC8E0."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
미래 또는 과거의 날짜 계산이 무엇인지 아세요? 간단히 말해, 특정 날짜에서 몇 일을 더하거나 빼서 새로운 날짜를 구하는 거예요. 이건 예약시스템, 마감일 추적, 기념일 알림 같은 기능에서 유용하죠.

## How to: (어떻게:)
```Python
from datetime import datetime, timedelta

# 오늘 날짜 구하기
today = datetime.now()

# 10일 후 날짜 계산
future_date = today + timedelta(days=10)
print("10일 후:", future_date.strftime("%Y-%m-%d"))

# 5일 전 날짜 계산
past_date = today - timedelta(days=5)
print("5일 전:", past_date.strftime("%Y-%m-%d"))
```

### Sample Output
```
10일 후: 2023-04-20
5일 전: 2023-04-05
```

## Deep Dive (심층 분석)
과거에는 `timedelta` 같은 편리한 도구가 없었어요. 개발자들은 원시적인 방법으로 날짜를 계산하곤 했죠. 파이썬은 이 과정을 `datetime` 모듈로 매끄럽게 해 줍니다. 이 모듈은 또한 시간대 관련 처리를 위해 `pytz` 같은 타 라이브러리와도 잘 작동해요. 날짜 계산은 내부적으로 날짜를 유닉스 타임스탬프(1970년 1월 1일부터 계산된 초)로 변환하고 연산 후 다시 날짜로 변환하는 방식으로 이루어집니다.

## See Also (참고 자료)
- datetime 모듈 공식 문서: https://docs.python.org/3/library/datetime.html
- pytz 라이브러리: http://pytz.sourceforge.net/
- timedelta 공식 문서: https://docs.python.org/3/library/datetime.html#timedelta-objects
