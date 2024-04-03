---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:58.635872-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: Lua\uB294 `os.date` \uBC0F `os.time`\
  \ \uD568\uC218\uC5D0 \uC758\uD574 \uC81C\uACF5\uB418\uB294 \uC81C\uD55C\uB41C \uAE30\
  \uB2A5 \uC678\uC5D0 \uB0A0\uC9DC \uBC0F \uC2DC\uAC04 \uC870\uC791\uC744 \uC704\uD55C\
  \ \uB0B4\uC7A5 \uC9C0\uC6D0\uC774 \uC5C6\uC2B5\uB2C8\uB2E4. \uADF8\uB7EC\uB098 \uC774\
  \uB7EC\uD55C \uAE30\uB2A5\uC744 \uAE30\uBCF8 \uD30C\uC2F1\uC744 \uC704\uD574 \uD65C\
  \uC6A9\uD560 \uC218 \uC788\uC73C\uBA70, \uB354 \uBCF5\uC7A1\uD55C \uC694\uAD6C \uC0AC\
  \uD56D\uC758 \uACBD\uC6B0 \uC678\uBD80 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778 `luadate`\u2026"
lastmod: '2024-03-13T22:44:55.430912-06:00'
model: gpt-4-0125-preview
summary: "Lua\uB294 `os.date` \uBC0F `os.time` \uD568\uC218\uC5D0 \uC758\uD574 \uC81C\
  \uACF5\uB418\uB294 \uC81C\uD55C\uB41C \uAE30\uB2A5 \uC678\uC5D0 \uB0A0\uC9DC \uBC0F\
  \ \uC2DC\uAC04 \uC870\uC791\uC744 \uC704\uD55C \uB0B4\uC7A5 \uC9C0\uC6D0\uC774 \uC5C6\
  \uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB0A0\uC9DC \uBD84\uC11D\uD558\uAE30"
weight: 30
---

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
