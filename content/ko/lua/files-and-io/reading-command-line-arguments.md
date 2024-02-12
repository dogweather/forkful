---
title:                "명령줄 인수 읽기"
date:                  2024-01-20T17:56:21.290587-07:00
model:                 gpt-4-1106-preview
simple_title:         "명령줄 인수 읽기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
명령줄 인수를 읽는 것은 프로그램에게 터미널이나 커맨드 프롬프트에서 데이터를 전달하는 방법입니다. 프로그래머는 사용자의 입력에 따라 다르게 반응하는 유연한 프로그램을 만들기 위해 이 기능을 사용합니다.

## How to: (어떻게:)
```Lua
-- myscript.lua
local arg1, arg2 = ...

print("Argument 1:", arg1)
print("Argument 2:", arg2)

-- 터미널에서 실행:
-- lua myscript.lua Hello Lua

-- 결과:
-- Argument 1: Hello
-- Argument 2: Lua
```

## Deep Dive (심층 분석)
명령줄 인수는 프로그램 실행에 초기 매개변수를 설정할 때부터 사용되었습니다. Lua에서 `...` (vararg 표현식)을 사용하여 인수를 불러옵니다. 이것은 모든 전달된 인수를 포착합니다. `arg` 전역 테이블은 스크립트명(`arg[0]`)과 다음 인수(`arg[1]`, `arg[2]`, 등)도 포함합니다.

대안으로, `arg` 테이블을 사용할 수도 있지만 이는 `...`보다 복잡합니다. `arg[n]`을 통해 특정 인수에 접근할 수 있습니다.

명령줄 인수 처리는 다양한 스크립트와 응용 프로그램에서 커스텀 인수를 처리하는 데 필수적입니다. 복잡한 인수 처리를 위해서는 외부 라이브러리를 사용할 수도 있습니다.

## See Also (더 보기)
- Lua 5.4 Reference Manual (명령줄 인수): https://www.lua.org/manual/5.4/manual.html#6.1
- `lua-users` wiki (명령줄 인수 처리): http://lua-users.org/wiki/CommandLineArguments
- GitHub에서 Lua Argument Parser 라이브러리 검색 (복잡한 인수 처리): https://github.com/search?q=lua+argument+parser
