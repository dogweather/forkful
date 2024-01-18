---
title:                "컴퓨터 프로그래밍에서 명령줄 인수 읽기"
html_title:           "Lua: 컴퓨터 프로그래밍에서 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령줄 인수 읽기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
커맨드 라인 인수를 읽는 것은 프로그래머가 커맨드 라인에서 사용자로부터 입력을 받아들이기 위해하는 작업입니다. 이를 통해 프로그램을 실행하는 동안 사용자가 제공하는 다양한 옵션 및 인수를 활용할 수 있습니다.

## 방법:
```Lua
--예제 1: 인수를 출력하는 간단한 프로그램
for i, v in ipairs(arg) do
  print("인수 " .. i .. ": " .. v)
end

--터미널에서 다음과 같이 실행
$ lua example.lua a b c
인수 1: a
인수 2: b
인수 3: c
```

```Lua
--예제 2: 인수를 활용하는 프로그램
local name = arg[1]
local age = arg[2]
print("안녕하세요, " .. name .. "님. 당신은 " .. age .. "살 이군요!")
```

```Lua
--터미널에서 다음과 같이 실행
$ lua example.lua Peter 25
안녕하세요, Peter님. 당신은 25살 이군요!
```

## 깊이 들어가기:
커맨드 라인 인수를 읽는 기능은 프로그래밍 언어마다 다양한 구현 방식을 가지고 있습니다. 대표적으로 C 언어의 argv 및 argc와 파이썬의 sys.argv 등이 있습니다. 루아에서는 arg 테이블을 통해 인수를 읽을 수 있습니다.

## 관련 자료:
- [루아 공식 문서](https://www.lua.org/manual/5.4/manual.html#6.9)
- [루아 인수 강의](https://www.youtube.com/watch?v=7V7Ge4woemA)