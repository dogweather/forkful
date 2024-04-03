---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:59.455367-07:00
description: "\uC5B4\uB5BB\uAC8C: Lua\uC5D0\uC11C\uB294 \uB514\uB809\uD1A0\uB9AC\uAC00\
  \ \uC874\uC7AC\uD558\uB294\uC9C0 \uC9C1\uC811 \uD655\uC778\uD560 \uC218 \uC788\uB294\
  \ \uB0B4\uC7A5 \uD568\uC218\uAC00 \uC5C6\uC73C\uBBC0\uB85C, \uD30C\uC77C \uC791\uC5C5\
  \uC744 \uC704\uD55C \uC778\uAE30 \uC788\uB294 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\
  \uB9AC\uC778 Lua File System(lfs) \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC790\uC8FC\
  \ \uC0AC\uC6A9\uD558\uAC8C \uB429\uB2C8\uB2E4. \uBA3C\uC800, Lua File System\uC774\
  \ \uC124\uCE58\uB418\uC5B4 \uC788\uB294\uC9C0 \uD655\uC778\uD558\uC2ED\uC2DC\uC624\
  .\u2026"
lastmod: '2024-03-13T22:44:55.438413-06:00'
model: gpt-4-0125-preview
summary: "Lua\uC5D0\uC11C\uB294 \uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\
  \uC9C0 \uC9C1\uC811 \uD655\uC778\uD560 \uC218 \uC788\uB294 \uB0B4\uC7A5 \uD568\uC218\
  \uAC00 \uC5C6\uC73C\uBBC0\uB85C, \uD30C\uC77C \uC791\uC5C5\uC744 \uC704\uD55C \uC778\
  \uAE30 \uC788\uB294 \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC778 Lua File System(lfs)\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC790\uC8FC \uC0AC\uC6A9\uD558\uAC8C \uB429\
  \uB2C8\uB2E4."
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
weight: 20
---

## 어떻게:
Lua에서는 디렉토리가 존재하는지 직접 확인할 수 있는 내장 함수가 없으므로, 파일 작업을 위한 인기 있는 타사 라이브러리인 Lua File System(lfs) 라이브러리를 자주 사용하게 됩니다.

먼저, Lua File System이 설치되어 있는지 확인하십시오. 아니라면, LuaRocks를 사용하여 일반적으로 설치할 수 있습니다:

```sh
luarocks install luafilesystem
```

그런 다음, 다음 예제를 사용하여 디렉토리의 존재 여부를 확인할 수 있습니다:

```lua
local lfs = require "lfs"

function directoryExists(directory)
    local attr = lfs.attributes(directory)
    return attr and attr.mode == "directory"
end

-- 특정 디렉토리가 존재하는지 확인
if directoryExists("/path/to/your/directory") then
    print("디렉토리가 존재합니다.")
else
    print("디렉토리가 존재하지 않습니다.")
end
```

이렇게 출력됩니다:

```
디렉토리가 존재합니다.
```

또는, 디렉토리가 존재하지 않는 경우:

```
디렉토리가 존재하지 않습니다.
```

이 접근 방식은 `lfs.attributes` 함수를 사용하여 경로의 속성을 가져옵니다. 경로가 존재하고 그 `mode` 속성이 `directory`라면 디렉토리의 존재를 확실히 확인할 수 있습니다.
