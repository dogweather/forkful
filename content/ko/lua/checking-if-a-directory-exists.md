---
title:                "디렉토리 존재 여부 확인하기"
date:                  2024-01-20T14:57:33.627298-07:00
html_title:           "Fish Shell: 디렉토리 존재 여부 확인하기"
simple_title:         "디렉토리 존재 여부 확인하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
디렉토리가 존재하는지 확인하는 건 파일 시스템에서 특정 폴더가 있는지 체크하는 과정입니다. 프로그래머는 파일을 저장하거나 읽기 전에 오류를 방지하기 위해 이를 수행합니다.

## How to: (어떻게 하나요?)
Lua에는 표준 라이브러리에서 직접 디렉토리의 존재를 확인할 수 있는 내장 함수가 없습니다. 그러나 `lfs` (LuaFileSystem) 라이브러리를 사용하면 가능합니다. 아래는 사용 예시입니다:

```Lua
local lfs = require "lfs"

function directoryExists(directory)
    local attributes = lfs.attributes(directory)
    if attributes and attributes.mode == "directory" then
        return true
    else
        return false
    end
end

-- 예시 사용:
local dirPath = "/path/to/directory"

if directoryExists(dirPath) then
    print("디렉토리가 존재해요!")
else
    print("디렉토리가 존재하지 않아요.")
end
```

## Deep Dive (심층 분석)
`lfs.attributes` 함수는 파일 또는 디렉토리에 대한 정보를 가져옵니다. 모드(mode)를 확인하여 'directory'인지 확인함으로써 존재 여부를 판단할 수 있습니다. Lua는 기본적으로 파일 시스템 작업에 대한 지원이 제한적이기 때문에, `lfs`와 같은 외부 라이브러리가 필요합니다.

안타깝게도 `lfs`는 설치해야 하는 추가 라이브러리입니다. LuaRocks 또는 유사한 패키지 관리자를 사용하여 설치할 수 있습니다. 대안으로, 특정 시스템 명령어를 호출하는 `os.execute`나 `io.popen`을 사용하는 방법도 있지만 크로스플랫폼 코드 작성 시 문제가 될 수 있습니다.

## See Also (관련 정보)
- LuaFileSystem 공식 문서: http://keplerproject.github.io/luafilesystem/
- LuaRocks: https://luarocks.org/
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/