---
title:                "문자열에서 날짜 분석하기"
date:                  2024-02-03T19:14:58.635872-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열에서 날짜 분석하기"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇인가 & 왜인가?
문자열에서 날짜를 파싱한다는 것은 날짜와 시간의 텍스트 표현을 Lua 프로그램 내에서 쉽게 조작, 저장 또는 비교할 수 있는 형식으로 변환하는 것을 말합니다. 프로그래머들은 일정 관리, 로깅, 시간 계산 등의 작업을 용이하게 하고 인간이 읽을 수 있는 날짜 형식과 컴퓨터가 효율적으로 처리할 수 있는 구조화된 데이터 유형 간의 격차를 메우기 위해 이 작업을 수행합니다.

## 어떻게 하나:
Lua는 `os.date` 및 `os.time` 함수에 의해 제공되는 제한된 기능 외에 날짜 및 시간 조작을 위한 내장 지원이 없습니다. 그러나 이러한 기능을 기본 파싱을 위해 활용할 수 있으며, 더 복잡한 요구 사항의 경우 외부 라이브러리인 `luadate` 라이브러리를 사용할 수 있습니다.

**`os.date` 및 `os.time` 사용하기:**
```lua
-- 인간이 읽을 수 있는 날짜를 타임스탬프로 변환하고 다시 변환하기
local dateString = "2023-09-21 15:00:00"
local pattern = "(%d+)-(%d+)-(%d+) (%d+):(%d+):(%d+)"
local year, month, day, hour, minute, second = dateString:match(pattern)

local timestamp = os.time({
  year = year,
  month = month,
  day = day,
  hour = hour,
  min = minute,
  sec = second
})

-- 타임스탬프를 다시 인간이 읽을 수 있는 형식으로 변환
local formattedDate = os.date("%Y-%m-%d %H:%M:%S", timestamp)
print(formattedDate)  -- 출력: 2023-09-21 15:00:00
```

**`luadate` 사용하기 (서드파티 라이브러리):**
`luadate`를 사용하기 위해서는 LuaRocks 또는 선택한 패키지 관리자를 통해 설치되어 있어야 합니다. `luadate`는 날짜와 시간의 파싱 및 조작 기능을 대폭 추가합니다.

```lua
local date = require('date')

-- 날짜 문자열을 직접 파싱
local parsedDate = date.parse("2023-09-21 15:00:00")
print(parsedDate:fmt("%Y-%m-%d %H:%M:%S"))  -- 출력: 2023-09-21 15:00:00

-- 기간 추가하기
local oneWeekLater = parsedDate:adddays(7)
print(oneWeekLater:fmt("%Y-%m-%d %H:%M:%S"))  -- 출력: 2023-09-28 15:00:00
```

`luadate` 라이브러리는 문자열에서 파싱, 형식 지정, 날짜에 대한 산술 연산 등, 날짜 작업을 훨씬 직관적이고 강력하게 만드는 방법을 제공하여 Lua에서 시간 데이터를 다루는 작업을 상당히 단순화합니다.
