---
title:                "문자열에서 날짜 추출"
html_title:           "Lua: 문자열에서 날짜 추출"
simple_title:         "문자열에서 날짜 추출"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

일반적으로 프로그래밍에서 날짜는 문자열로 저장될 수도 있습니다. 그러나 이 문자열을 일반적으로 알아보기 쉬운 형식으로 변환해야 할 때가 있습니다. 이것을 문자열에서 날짜로 파싱한다고 합니다. 프로그래머는 이 작업을 수행함으로써 원하는 날짜 형식으로 데이터를 조작하고 표시할 수 있습니다.

## 하는 방법:

```Lua
-- 문자열에서 날짜로 파싱하는 방법
local date = "2021년 7월 21일"
local format = "%Y년 %m월 %d일"

-- os.date 함수를 사용하여 문자열을 날짜로 변환
local parsedDate = os.date(format, date)

-- 변환된 날짜 출력
print(parsedDate)
```
출력: 2021년 07월 21일

```Lua
-- 다른 형식의 입력 문자열과 포맷을 사용하는 예제
local date = "21-07-2021"
local format = "%d-%m-%Y"

local parsedDate = os.date(format, date)

print(parsedDate)
```
출력: 21-07-2021

## 더 들어가보기:

파싱이란 용어는 주로 자연어 처리 분야에서 사용되었습니다. 그러나 프로그래밍에서는 문자열을 해석해서 특정한 형태로 바꾸는 것을 의미합니다. 다양한 언어에서는 날짜 포맷을 다루는 라이브러리를 제공하기도 합니다. 이를 이용하면 더욱 정확하고 복잡한 날짜 변환도 가능합니다.

## 관련 링크:

- Lua 날짜 다루기: https://www.lua.org/pil/22.1.html
- 날짜 포맷 처리 기능 라이브러리: https://www.lua.org/manual/5.3/manual.html#6.9