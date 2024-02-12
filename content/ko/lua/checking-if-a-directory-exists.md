---
title:                "디렉토리가 존재하는지 확인하기"
aliases:
- ko/lua/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:59.455367-07:00
model:                 gpt-4-0125-preview
simple_title:         "디렉토리가 존재하는지 확인하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉토리가 존재하는지 확인하는 것은 파일 시스템과 상호 작용하는 스크립트를 작성할 때 기본적인 작업으로, 프로그램이 유효한 경로에서 작동하도록 하고 존재하지 않는 디렉토리와 관련된 오류를 방지하는 데 필수적입니다. 이 작업은 디렉토리에서 새 파일을 생성하거나, 그것들로부터 읽거나, 디렉토리별 작업을 안전하게 수행하는 데 중요합니다.

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
