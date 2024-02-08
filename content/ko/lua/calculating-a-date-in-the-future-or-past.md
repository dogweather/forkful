---
title:                "미래나 과거의 날짜 계산하기"
aliases:
- ko/lua/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:40.264920-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 미래나 과거로 계산한다는 것은 지정된 시간을 기준으로 특정 기간만큼 시간을 더하거나 빼는 것을 말합니다. 프로그래머들은 유효기간 계산, 예약 시스템, 이벤트 스케줄링 등을 위해 이를 사용합니다.

## How to (방법)
Lua에서는 `os.date`와 `os.time` 함수를 사용해 날짜를 계산합니다.

```Lua
-- 오늘 날짜 가져오기
local today = os.time()
print("오늘: " .. os.date("%Y-%m-%d", today))

-- 7일 더하기
local seven_days_later = os.time({year=os.date("%Y"), month=os.date("%m"), day=os.date("%d") + 7})
print("7일 후: " .. os.date("%Y-%m-%d", seven_days_later))

-- 30일 빼기
local thirty_days_ago = os.time({year=os.date("%Y"), month=os.date("%m"), day=os.date("%d") - 30})
print("30일 전: " .. os.date("%Y-%m-%d", thirty_days_ago))
```
출력 결과:
```
오늘: 2023-04-12
7일 후: 2023-04-19
30일 전: 2023-03-13
```

## Deep Dive (심층 분석)
날짜 계산은 UNIX 시간(epoch time) 개념에서부터 시작합니다. 1970년 1월 1일부터 초를 세는 방식이죠. Lua의 `os.time` 함수는 이 방식을 사용해 시간을 표현합니다. 대안으로 `os.date` 없이 날짜를 계산할 수도 있으나, 코드 복잡성이 늘어납니다. 상황에 따라 LuaRocks 같은 외부 라이브러리를 사용하여 더 복잡한 날짜 연산을 할 수 있습니다.

## See Also (관련 자료)
- Lua 표준 라이브러리 문서: [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#6.9)
- UNIX 시간과 작동 원리에 대한 추가 정보: [UNIX Time Wikipedia Entry](https://en.wikipedia.org/wiki/Unix_time)
