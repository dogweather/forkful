---
date: 2024-01-20 17:51:31.234441-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uAC00) Lua\uC5D0\
  \uC11C \uBB38\uC790\uC5F4 \uB0B4\uC0BD\uC744 \uD560 \uB54C\uB294 `string.format`\
  \ \uD568\uC218\uB098 concat operator (`..`)\uB97C \uC0AC\uC6A9\uD574\uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.090252-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uC0AC\uC6A9\uD558\uB294\uAC00) Lua\uC5D0\uC11C \uBB38\
  \uC790\uC5F4 \uB0B4\uC0BD\uC744 \uD560 \uB54C\uB294 `string.format` \uD568\uC218\
  \uB098 concat operator (`..`)\uB97C \uC0AC\uC6A9\uD574\uC694."
title: "\uBB38\uC790\uC5F4 \uBCF4\uAC04\uD558\uAE30"
weight: 8
---

## How to: (어떻게 사용하는가)
Lua에서 문자열 내삽을 할 때는 `string.format` 함수나 concat operator (`..`)를 사용해요.

```Lua
local name = "세계"
local greeting = string.format("안녕하세요, %s!", name)
print(greeting)  -- 출력: 안녕하세요, 세계!

local temperature = 23
local weather = "지금 기온은 "..temperature.."도 입니다."
print(weather)  -- 출력: 지금 기온은 23도 입니다.
```

## Deep Dive (깊이 있는 정보)
Lua에서 공식적으로 문자열 내삽 기능을 제공하는 것은 아니에요. 하지만 `string.format`을 통해 C's printf 스타일의 내삽을 할 수 있죠. Lua 5.1 버전부터 사용 가능합니다. 이외에도 문자열 연결(concatenation) 연산자인 `..`을 간단히 사용할 수 있어요. 내삽 방식에는 성능상 차이가 있을 수 있으니 상황에 따라 적절히 선택하는 게 중요합니다.

## See Also (관련 자료)
- Lua 공식 문서: https://www.lua.org/manual/5.4/manual.html#6.4.1
- `string.format`에 대한 추가 정보: http://www.lua.org/pil/20.3.html
- String concatenation in Lua: http://www.lua.org/pil/11.6.html
