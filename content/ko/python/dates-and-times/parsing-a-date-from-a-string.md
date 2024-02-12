---
title:                "문자열에서 날짜 분석하기"
aliases:
- /ko/python/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:09.658496-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 날짜 분석하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?
문자열에서 날짜를 파싱한다는 것은 텍스트로 된 날짜와 시간 정보를 datetime 객체 또는 동등한 구조화된 형식으로 변환하는 것을 말합니다. 이는 주로 날짜 산술, 비교 및 포맷팅 작업을 언어 및 지역에 구애받지 않는 방식으로 가능하게 하기 위해 수행됩니다. 프로그래머들은 로그, 사용자 입력 또는 외부 소스에서 추출한 시간 데이터를 효율적으로 처리하고 조작하기 위해 이 작업을 합니다.

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
