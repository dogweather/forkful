---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:59.455367-07:00
description: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\
  \uC778\uD558\uB294 \uAC83\uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uACFC \uC0C1\uD638\
  \ \uC791\uC6A9\uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uC791\uC131\uD560 \uB54C\
  \ \uAE30\uBCF8\uC801\uC778 \uC791\uC5C5\uC73C\uB85C, \uD504\uB85C\uADF8\uB7A8\uC774\
  \ \uC720\uD6A8\uD55C \uACBD\uB85C\uC5D0\uC11C \uC791\uB3D9\uD558\uB3C4\uB85D \uD558\
  \uACE0 \uC874\uC7AC\uD558\uC9C0 \uC54A\uB294 \uB514\uB809\uD1A0\uB9AC\uC640 \uAD00\
  \uB828\uB41C \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uB294 \uB370 \uD544\uC218\uC801\
  \uC785\uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB514\uB809\uD1A0\uB9AC\uC5D0\uC11C\
  \ \uC0C8 \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uAC70\uB098, \uADF8\uAC83\uB4E4\uB85C\
  \uBD80\uD130\u2026"
lastmod: 2024-02-19 22:05:14.350848
model: gpt-4-0125-preview
summary: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uB294 \uAC83\uC740 \uD30C\uC77C \uC2DC\uC2A4\uD15C\uACFC \uC0C1\uD638 \uC791\
  \uC6A9\uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uC791\uC131\uD560 \uB54C \uAE30\
  \uBCF8\uC801\uC778 \uC791\uC5C5\uC73C\uB85C, \uD504\uB85C\uADF8\uB7A8\uC774 \uC720\
  \uD6A8\uD55C \uACBD\uB85C\uC5D0\uC11C \uC791\uB3D9\uD558\uB3C4\uB85D \uD558\uACE0\
  \ \uC874\uC7AC\uD558\uC9C0 \uC54A\uB294 \uB514\uB809\uD1A0\uB9AC\uC640 \uAD00\uB828\
  \uB41C \uC624\uB958\uB97C \uBC29\uC9C0\uD558\uB294 \uB370 \uD544\uC218\uC801\uC785\
  \uB2C8\uB2E4. \uC774 \uC791\uC5C5\uC740 \uB514\uB809\uD1A0\uB9AC\uC5D0\uC11C \uC0C8\
  \ \uD30C\uC77C\uC744 \uC0DD\uC131\uD558\uAC70\uB098, \uADF8\uAC83\uB4E4\uB85C\uBD80\
  \uD130\u2026"
title: "\uB514\uB809\uD1A0\uB9AC\uAC00 \uC874\uC7AC\uD558\uB294\uC9C0 \uD655\uC778\
  \uD558\uAE30"
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
