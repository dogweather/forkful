---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?

텍스트 파일 읽기는 프로그램이 파일 안의 내용을 읽어오는 것입니다. 이를 통해 프로그래머들은 데이터를 가져오거나, 필요한 정보를 빠르게 찾을 수 있습니다.

## 어떻게 할까?

Lua에서 텍스트 파일을 읽는 기본적인 단계는 아래와 같습니다.

```Lua
-- 파일 열기
local file = io.open("example.txt", "r")

-- 파일 읽기
local content = file:read("*a")

-- 출력하기
print(content)

-- 파일 닫기
file:close()
```
이 코드는 "example.txt"라는 파일의 모든 내용을 읽고, 그 내용을 출력합니다. `*a` 옵션은 파일의 모든 내용을 한 번에 읽습니다.

## 깊은 시각 

텍스트 파일을 읽는 것은 오래전부터 많은 프로그래밍 언어에서 기본적으로 제공되는 기능이었습니다. Lua에서도 이런 기능을 제공하며, 상세한 구현 방식은 운영체제와 IO 라이브러리에 따라 다릅니다. 

Lua의 대안으로는 파일을 한 줄씩 읽는 방법, 또는 특정 크기로 분할하여 읽는 방법 등이 있습니다. 아래 예시는 한 줄씩 읽는 방법을 보여줍니다.

```Lua
local file = io.open("example.txt", "r")

for line in file:lines() do
    print(line)
end

file:close()
```
## 참고 자료

Lua 파일 IO에 대한 자세한 정보는 아래 링크에서 참조하실 수 있습니다.

- Official Lua 5.4 Manual : [I/O Library](https://www.lua.org/manual/5.4/manual.html#6.8)
- Definitions and uses : [File I/O](https://learnxinyminutes.com/docs/lua/)
- Tutorials Point : [Lua - File I/O](https://www.tutorialspoint.com/lua/lua_file_io.htm)