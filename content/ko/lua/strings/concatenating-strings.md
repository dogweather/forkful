---
date: 2024-01-20 17:35:38.515720-07:00
description: "How to: (\uBC29\uBC95:) \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 Lua\uAC00\
  \ \uCC98\uC74C \uB9CC\uB4E4\uC5B4\uC84C\uC744 \uB54C\uBD80\uD130 \uC788\uC5C8\uB358\
  \ \uAE30\uB2A5\uC785\uB2C8\uB2E4. `..` \uC624\uD37C\uB808\uC774\uD130\uB85C \uAC04\
  \uB2E8\uD788 \uC4F8 \uC218 \uC788\uB294 \uBC18\uBA74, \uB9CE\uC740 \uC591\uC758\
  \ \uB370\uC774\uD130\uB97C \uC5F0\uACB0\uD560 \uB54C\uB294 `table.concat` \uD568\
  \uC218\uB97C \uC0AC\uC6A9\uD558\uB294 \uAC83\uC774 \uD6A8\uC728\uC801\uC785\uB2C8\
  \uB2E4. \uB0B4\uBD80\uC801\uC73C\uB85C, Lua\uB294 \uBB38\uC790\uC5F4\uC744 \uBCC0\
  \uD558\uC9C0 \uC54A\uB294\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.096457-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) \uBB38\uC790\uC5F4 \uC5F0\uACB0\uC740 Lua\uAC00 \uCC98\uC74C\
  \ \uB9CC\uB4E4\uC5B4\uC84C\uC744 \uB54C\uBD80\uD130 \uC788\uC5C8\uB358 \uAE30\uB2A5\
  \uC785\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uC5F0\uACB0\uD558\uAE30"
weight: 3
---

## How to: (방법:)
```Lua
-- 문자열 연결 예시
local greeting = "안녕"
local name = "세상"
local message = greeting .. ", " .. name .. "!"

print(message)  -- '안녕, 세상!'
```

```Lua
-- 문자열과 숫자 연결
local temperature = 25
local unit = "도씨"
local weatherReport = "오늘 기온은 " .. temperature .. unit .. "입니다."

print(weatherReport)  -- '오늘 기온은 25도씨입니다.'
```

```Lua
-- table.concat 함수를 이용한 배열 문자열 연결
local colors = {"빨강", "노랑", "파랑"}
local list = table.concat(colors, ", ")

print(list)  -- '빨강, 노랑, 파랑'
```

## Deep Dive (심화 학습)
문자열 연결은 Lua가 처음 만들어졌을 때부터 있었던 기능입니다. `..` 오퍼레이터로 간단히 쓸 수 있는 반면, 많은 양의 데이터를 연결할 때는 `table.concat` 함수를 사용하는 것이 효율적입니다. 내부적으로, Lua는 문자열을 변하지 않는 값(immutable)으로 다루기 때문에 새로운 문자열이 만들어질 때마다 메모리상에 새로운 공간을 할당받습니다. 따라서 큰 데이터를 다룰 때는 성능에 주의해야 합니다.

`..` 오퍼레이터 대신 `..=` 연산자를 사용하여 문자열에 연속적으로 덧붙이는 것도 가능하지만, 이는 새로운 문법이 들어오면서 사용할 수 있게 된 기능입니다. 기존 코드베이스와의 호환성을 고려해야 합니다.

## See Also (참고자료)
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/
- Programming in Lua (first edition): https://www.lua.org/pil/contents.html
- Lua-users wiki: Concatenation: http://lua-users.org/wiki/StringConcatenation
