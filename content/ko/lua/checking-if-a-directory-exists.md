---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉토리 '존재 확인'이란 특정 디렉토리가 이미 존재하는지 확인하는 것을 의미합니다. 프로그래머가 이것을 하는 이유는 대체로 일관된 파일 구조를 유지하고, 파일 시스템 에러를 방지하기 위함입니다.

## 사용법:

다음은 Lua에서 디렉토리 존재 여부를 확인하는 코드입니다.
```Lua
local lfs = require("lfs")

function is_direxist(dir)
    if (lfs.attributes(dir, "mode") == "directory") then
        return true
    else
        return false
    end
end

print(is_direxist("/path/to/directory"))
```
이 코드를 실행하면, "/path/to/directory"가 존재하는지 결과가 출력됩니다.

## 디피 다이브:

디렉토리 '존재 확인'은 컴퓨터 역사 상 중요한 부분을 차지하였으며, 다양한 언어에서 다양한 방법으로 구현되었습니다. Lua에서는 최소한의 처리를 통해 직관적으로 그 기능을 수행할 수 있습니다.

대안 방법으로, os.execute를 사용하여 파일 시스템 명령을 직접 실행할 수도 있지만, 유효성 검사와 보안 문제로 인해 이 사용법은 비권장입니다.

lfs.attributes 함수는 주어진 경로에 대한 속성을 가져 오거나 실패하면 nil을 반환합니다. 'mode'와 같은 속성은 문자열로 반환되는데, 이 값이 "directory"인 경우 해당 경로는 디렉토리임을 의미합니다.

## 참고 자료:

* Lua manual: [https://www.lua.org/manual](https://www.lua.org/manual)