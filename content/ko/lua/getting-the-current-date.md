---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:15:56.188006-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
현재 날짜를 얻는 것은 시스템의 날짜와 시간을 확인하는 과정입니다. 프로그래머들은 로깅, 타임스탬프, 기능 제한 같은 작업을 위해 이를 사용합니다.

## How to: (방법:)
```Lua
-- 현재 날짜와 시간 얻기
local current_time = os.date("*t") -- os.date() 함수 사용

-- 현재 연, 월, 일 출력하기
print("Year:", current_time.year)
print("Month:", current_time.month)
print("Day:", current_time.day)

-- 바로 사용자에게 날짜 보여주기
print("Today is:", os.date("%Y-%m-%d"))
```

**Sample Output:**
```
Year: 2023
Month: 4
Day: 10
Today is: 2023-04-10
```

## Deep Dive (심층 분석)
Lua에서 `os.date()` 함수는 ANSI C의 `strftime()` 함수를 기반으로 합니다. 1993년에 처음으로 Lua가 등장했을 때부터 날짜와 시간 기능은 중요한 부분이었습니다. Lua에서는 간단하지만 유연한 방식으로 날짜를 표현하고 처리합니다.

`os.date()`에 넘기는 포맷 문자열은 출력을 조정하는 데 사용됩니다. 기본값인 `*t`는 테이블 형태로 날짜의 모든 정보를 제공합니다. 대안으로는 `os.time()` 함수가 있는데, 이 함수는 현재 시간을 초 단위의 숫자로 반환합니다.

Lua는 플랫폼 간 차이를 처리하기 위해 내부적으로 시간 관련 기능을 구현합니다. 그러나 시계의 정확성은 실행 중인 시스템의 시간 설정에 따라 달라질 수 있습니다.

## See Also (더 보기)
- Lua 5.4 Reference Manual: `os.date`에 대한 자세한 내용은 [Lua 5.4 Manual](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)을 참조하세요.
- Lua-users Wiki: 다양한 날짜와 시간 처리 방법에 대해 더 배우고 싶으시면 [Lua-users Wiki](http://lua-users.org/wiki/DateTime)를 방문해보세요.
- Programming in Lua: Lua 프로그래밍의 기본을 배우기 위한 책 [Programming in Lua](https://www.lua.org/pil/)를 읽어보세요.