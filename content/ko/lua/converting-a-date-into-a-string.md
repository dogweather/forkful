---
title:                "날짜를 문자열로 변환하기"
html_title:           "Lua: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

날짜를 문자열로 변환하는 것은 날짜 데이터를 다룰 때 많이 사용됩니다. 프로그래머는 날짜를 다루는 과정에서 날짜를 보다 효율적으로 표현하거나 저장하기 위해 문자열로 변환하는 경우가 있습니다.

## 방법:

```Lua
-- 현재 시간을 문자열로 변환하는 예시
local current_time = os.date("%c")
print(current_time)
-- 결과: Thu Apr 22 09:32:26 2021
```

```Lua
-- 다른 언어 형식으로 날짜를 문자열로 변환하는 예시
local current_time = os.date("%x")
print(current_time)
-- 결과: 04/22/21
```

```Lua
-- 특정 날짜를 문자열로 변환하는 예시
local date = os.date("*t", os.time({year = 2021, month = 4, day = 22}))
print(string.format("%02d/%02d/%d", date.month, date.day, date.year))
-- 결과: 04/22/2021
```

## 깊이 파고들기:

### 역사적 배경:

날짜를 문자열로 변환하기 위해 사용된 첫 번째 프로그래밍 언어는 FORTRAN이었습니다. 그 이후 많은 언어에서 동일한 방법이 사용되었고, Lua 역시 그 예외는 아닙니다.

### 대안:

날짜를 문자열로 변환하는 것 외에도 날짜를 정수로 표현하는 방법이 있습니다. 이는 날짜를 계산하거나 저장하는 데 더 나은 유연성을 제공할 수 있습니다.

### 구현 세부 사항:

Lua에서 날짜를 문자열로 변환하는 기능은 내장 함수인 os.date()를 사용합니다. 이 함수의 첫 번째 매개변수에는 변환하고자하는 날짜 형식을 지정해주고, 두 번째 매개변수에는 변환할 날짜를 지정해주면 됩니다.

## 관련 자료:

- [Lua의 os.date() 함수에 대한 공식 문서]("https://www.lua.org/manual/5.3/manual.html#6.9")
- [Lua에서 날짜와 시간 다루기에 대한 부록]("https://www.lua.org/pil/22.1.html")