---
date: 2024-01-20 17:36:56.023654-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uAC83\uC740 \uB0A0\uC9DC \uB370\uC774\uD130\uB97C \uBB38\uC790 \uD615\uD0DC\uB85C\
  \ \uD45C\uD604\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uAC00 \uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294\
  \ \uC774\uC720\uB294 \uC0AC\uC6A9\uC790\uC5D0\uAC8C \uCE5C\uC219\uD55C \uD615\uC2DD\
  \uC73C\uB85C \uB0A0\uC9DC\uB97C \uBCF4\uC5EC\uC8FC\uAC70\uB098 \uB370\uC774\uD130\
  \uB97C \uC800\uC7A5, \uB85C\uAE45\uD558\uACE0 \uD1B5\uC2E0\uD558\uAE30 \uC704\uD574\
  \uC11C\uC785\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.434177-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uB294 \uAC83\
  \uC740 \uB0A0\uC9DC \uB370\uC774\uD130\uB97C \uBB38\uC790 \uD615\uD0DC\uB85C \uD45C\
  \uD604\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
weight: 28
---

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환하는 것은 날짜 데이터를 문자 형태로 표현하는 과정입니다. 프로그래머가 날짜를 문자열로 변환하는 이유는 사용자에게 친숙한 형식으로 날짜를 보여주거나 데이터를 저장, 로깅하고 통신하기 위해서입니다.

## How to (방법)
```Lua
-- 현재 날짜와 시간 가져오기
local current_time = os.date("*t")

-- 날짜와 시간을 문자열로 변환하기
local date_string = string.format("%04d-%02d-%02d %02d:%02d:%02d", 
                                  current_time.year, current_time.month, current_time.day, 
                                  current_time.hour, current_time.min, current_time.sec)

print(date_string) -- 출력 예: 2023-04-05 15:26:08
```

```Lua
-- 간단한 날짜 포맷 (`os.date` 사용)
local date_simple = os.date("%Y-%m-%d")
print(date_simple) -- 출력 예: 2023-04-05
```

## Deep Dive (심층 분석)
Lua에서 날짜를 문자열로 변환하는 기능은 주로 `os.date` 함수를 사용합니다. 이 함수는 ISO C 함수 `strftime`을 기반으로 하며, 다양한 포맷 지정자를 통해 원하는 날짜 형식을 얻을 수 있습니다. 이 방법은 1970년대부터 UNIX 시간과 C 표준 라이브러리에서 사용되어 왔습니다. 대안으로는 `os.time` 함수를 사용하여 시간을 초 단위로 먼저 구한 뒤 이를 다시 문자열로 변환하는 방법도 있습니다. 구현상의 세부 사항에 있어 Lua는 내부적으로 C의 `strftime`을 활용하므로 Lua의 날짜 및 시간 함수들은 C 표준에 크게 의존합니다.

## See Also (함께 보기)
- Lua 공식 문서의 os.date(): http://www.lua.org/manual/5.4/manual.html#pdf-os.date
- Lua 공식 문서의 os.time(): http://www.lua.org/manual/5.4/manual.html#pdf-os.time
- strftime 포맷 지정자에 대한 C 라이브러리 문서: https://www.cplusplus.com/reference/ctime/strftime/
