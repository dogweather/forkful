---
title:                "문자열 대문자화"
html_title:           "Lua: 문자열 대문자화"
simple_title:         "문자열 대문자화"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

문자열 대문자화란 문자열의 모든 알파벳 문자를 대문자로 변환하는 것입니다. 이 작업은 일관성 유지와 검색 편의성을 높이기 위해 프로그래머들이 자주 사용합니다.

## 어떻게:

Lua로 문자열 끝을 대문자로 변환하는 방법을 보여주겠습니다. 아래 코드를 확인해 보세요:

```Lua
function toUpper(str)
  return (str:gsub("^%l", string.upper))
end
print(toUpper("hello world"))
```

이 코드는 "hello world"를 "Hello World"로 변환합니다.

## Deeper Dive:

(1) Historically, 대문자화는 컴퓨터의 초기 문자 집합인 ASCII에서 시작되었습니다. 이것은 간단히 각 문자에 대한 ASCII 코드 값을 변환함으로써 이루어졌습니다.

(2) 문자열 대문자화에는 여러 가지 방법이 있습니다. `string.upper()`는 문자열의 모든 문자를 대문자로 변환하는 데 사용하는 또 다른 Lua 함수입니다.

(3) Lua에서 대문자화는 `string.upper()`, `string.gsub` 등의 기본 제공 문자열 함수를 활용하여 가능합니다. 이러한 함수들은 내부적으로 각 문자의 ASCII 코드 값을 변환하여 대문자화를 수행합니다.

## 참고:

- [Lua string manipulation](https://www.tutorialspoint.com/lua/lua_strings.htm)
- [Lua string functions](https://www.lua.org/pil/11.html)
- [ASCII Code - The extended ASCII table](https://www.ascii-code.com/)