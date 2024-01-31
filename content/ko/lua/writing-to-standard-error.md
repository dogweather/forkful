---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
simple_title:         "표준 오류로 쓰기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
프로그램에서 표준 에러는 오류 메시지를 출력하는 통로다. 프로그래머들이 이것을 사용하는 이유는 오류를 분류하고, 로그와 사용자 피드백을 관리하기 위해서다.

## How to: (방법)
Lua에서 표준 에러로 쓰기 위해 `io.stderr:write()`를 사용한다. 기본 출력(`print()`) 대신에, 에러 메시지는 `io.stderr:write()`를 사용해서 표련된다.

```Lua
-- 에러 메시지를 표준 에러로 출력
io.stderr:write("에러 발생!\n")

-- 출력 예시:
-- 에러 발생!
```

## Deep Dive (심층 분석)
Early computers had no way to differentiate between normal output and error messages. 표준 에러(stream) 개념은 나중에 UNIX 시스템에서 발전했다. Lua에서는 기본적으로 `io.stderr`이 표준 에러로 연결돼 있다. 출력과 에러 메시지를 다르게 다루어야 할 때 사용한다. `io.stderr`는 버퍼링되지 않아 즉시 에러를 출력한다.

## See Also (참고자료)
- Lua `io` 라이브러리 문서: https://www.lua.org/manual/5.4/manual.html#6.8
- UNIX 표준 출력과 표준 에러의 이해: https://en.wikipedia.org/wiki/Standard_streams
