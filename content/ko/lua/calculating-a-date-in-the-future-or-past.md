---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "Lua: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 뭐고 왜?
날짜를 미래 또는 과거로 계산하는 것은 컴퓨터 프로그래밍에서 일반적인 작업입니다. 프로그래머들은 특정 날짜에 대한 정보를 조작하거나 계산하기 위해 이 작업을 수행합니다.

# 하는 법:
```Lua 
-- 오늘 날짜 계산하기
local today = os.date("%d/%m/%Y")
print(today)

-- 내일 날짜 계산하기
local tomorrow = os.date("%d/%m/%Y", os.time() + 86400)
print(tomorrow)
```
```
19/10/2020
20/10/2020 
```

# 깊이 공부:
날짜를 계산하는 것은 우리가 현대 컴퓨터 프로그래밍에서 사용하는 많은 다른 기능들 중 하나입니다. 예전에는 달력을 만들거나 주기적인 일들을 추적하기 위해 수학적인 계산을 사용했습니다. 하지만 지금은 프로그래밍 언어 내장 함수를 사용하여 훨씬 쉽게 날짜를 처리할 수 있습니다.

# 참고:
[Lua Date and Time Functions](http://www.lua.org/pil/22.1.html)
[Exploring the Date and Time Functions in Lua](https://www.tutorialspoint.com/lua/lua_date_time.htm)