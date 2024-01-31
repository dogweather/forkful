---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 쓰기란 데이터를 일반 텍스트 형태로 파일에 저장하는 것입니다. 프로그래머들은 설정, 로그 파일 생성, 데이터 저장 등을 위해 이 작업을 수행합니다.

## How to: (어떻게 하나요?)
```Lua
-- 파일 쓰기 예제
local file = io.open("example.txt", "w")  -- "example.txt" 파일을 쓰기 모드로 열기
file:write("안녕하세요, Lua에서 파일을 쓰는 법을 배우고 있습니다.")  -- 텍스트 쓰기
file:close()  -- 파일 닫기
```

출력 예시 파일 "example.txt":
```
안녕하세요, Lua에서 파일을 쓰는 법을 배우고 있습니다.
```

## Deep Dive (깊게 파기)
- 히스토리: Lua는 1993년에 만들어졌으며, 파일 시스템 작업은 시작부터 포함되었습니다.
- 대안: `io.open` 외에 `io.output`, `file:flush` 같은 내장 함수도 있습니다.
- 실행 세부 사항: Lua는 내부적으로 C의 stdio 라이브러리를 사용하여 파일을 쓰고 있습니다.

## See Also (더 보기)
- Lua 파일 시스템에 관한 공식 문서: [http://www.lua.org/manual/5.4/manual.html#6.8](http://www.lua.org/manual/5.4/manual.html#6.8)
- Lua 커뮤니티 포럼: [https://www.lua.org/lua-l.html](https://www.lua.org/lua-l.html)
- Lua 파일 입출력 튜토리얼: [https://www.tutorialspoint.com/lua/lua_file_io.htm](https://www.tutorialspoint.com/lua/lua_file_io.htm)
