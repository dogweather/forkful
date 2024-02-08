---
title:                "문자열의 길이 찾기"
aliases:
- ko/lua/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:52.317372-07:00
model:                 gpt-4-1106-preview
simple_title:         "문자열의 길이 찾기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 해요?
문자열의 길이를 찾는 것은, 그 문자열이 몇 개의 문자로 이루어져 있는지 알아내는 과정입니다. 프로그래머들은 데이터 처리, 입력 검증, UI 디자인 등의 이유로 이를 수행합니다.

## 어떻게 해요?
```Lua
local exampleString = "안녕하세요!"
print(#exampleString)  -- 출력: 6
```

```Lua
local function countString(str)
    return #str
end

print(countString("루아 프로그래밍"))  -- 출력: 11
```

## 자세히 알아보기
문자열 길이를 찾는 것은 프로그래밍의 오래된 문제입니다. Lua에서는 `#` 연산자로 간단히 해결할 수 있는 반면, 다른 언어에서는 `length()` 함수나 다른 메서드를 사용하곤 합니다. Lua 5.3부터는 UTF-8 문자열에 대해 `utf8.len` 함수를 사용하여 정확한 문자 수를 얻을 수 있습니다. 반면, `#` 연산자는 바이트 단위의 길이를 반환하기 때문에, ASCII 문자가 아닌 경우에는 주의가 필요합니다.

```Lua
local utf8String = "안녕하세요!"
print(utf8.len(utf8String))  -- 출력: 6
print(#utf8String)           -- 출력: 18 (올바르지 않은 길이)
```

## 참고할만한 것들
- Lua 5.4 Reference Manual: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- Programming in Lua (first edition): [https://www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
