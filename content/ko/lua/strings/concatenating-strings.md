---
title:                "문자열 연결하기"
date:                  2024-01-20T17:35:38.515720-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열 연결하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
문자열 연결은 두 개 이상의 문자열을 붙여서 하나의 긴 문자열을 만드는 것입니다. 이것은 데이터 포맷을 조정하거나, 출력 메시지를 구성할 때 필요합니다.

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