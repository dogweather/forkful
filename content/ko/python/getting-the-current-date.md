---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:16:14.479247-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"

category:             "Python"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
현재 날짜를 얻는 것과 그 필요성
프로그래밍에서 현재 날짜를 얻는다는 것은 단순히 오늘의 날짜를 파악하는 것을 의미합니다. 이 정보는 로깅, 데이터 타임스탬핑, 기능 스케줄링 등 다양한 상황에서 필수적입니다.

## How to:
코드 예제 및 샘플 출력

```Python
from datetime import date

# 현재 날짜 얻기
today = date.today()

# 날짜를 문자열로 출력하기: yyyy-mm-dd
print(f"오늘의 날짜: {today}")
```

출력:
```
오늘의 날짜: 2023-04-01
```

```Python
import datetime

# 현재 날짜와 시각 얻기
now = datetime.datetime.now()

# 출력 형식: yyyy-mm-dd hh:mm:ss.ffffff
print(f"현재 시각: {now}")
```

출력:
```
현재 시각: 2023-04-01 15:20:12.463374
```

## Deep Dive
현재 날짜를 얻는 상세한 정보

현재 날짜와 시간은 컴퓨터 시스템의 내부 클록을 기반으로 합니다. 파이썬은 내장된 `datetime` 모듈을 통해 이 정보에 쉽게 접근할 수 있게 해줍니다. 이 모듈은 1990년대 후반에 파이썬에 도입되었고 시간을 다루는 데에 표준적인 방법을 제공합니다.

대안으로, `time` 모듈을 사용해 유닉스 타임스탬프를 얻을 수 있으며, 이는 1970년 1월 1일부터의 총 초를 나타냅니다. 또한 `calendar` 모듈을 사용하여 더 복잡한 날짜 계산을 수행할 수 있습니다.

또한, 파이썬의 시간 함수들은 운영체제의 시간 설정과 시계를 따릅니다. 이는 시스템의 타임존 설정에 따라 결과가 달라질 수 있다는 것을 의미합니다.

## See Also
관련 자료 링크

- 파이썬 공식 문서 `datetime` 모듈:
  [https://docs.python.org/3/library/datetime.html](https://docs.python.org/3/library/datetime.html)
- 파이썬 `time` 모듈에 대한 개요:
  [https://docs.python.org/3/library/time.html](https://docs.python.org/3/library/time.html)
- 파이써의 `calendar` 모듈 사용 방법:
  [https://docs.python.org/3/library/calendar.html](https://docs.python.org/3/library/calendar.html)
