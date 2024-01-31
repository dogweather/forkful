---
title:                "문자열 대문자로 변환하기"
date:                  2024-01-19
simple_title:         "문자열 대문자로 변환하기"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## 무엇이며 왜 사용하는가?
문자열을 대문자로 만든다는 건 모든 글자를 대문자로 변환하는 것입니다. 가독성을 높이거나, 프로그래밍에서 상수 값을 표현할 때 이를 사용합니다.

## How to:
## 방법:
```Lua
local text = "lua programming"
local capitalized_text = text:upper()

print(capitalized_text)  -- Output: LUA PROGRAMMING
```

## Deep Dive
## 심층 분석:
문자열을 대문자로 변환하는 기능은 프로그래밍이 시작된 이래로 많이 사용되어왔습니다. 본질적으로 문자열 처리는 프로그래밍의 핵심 요소며, Lua에서는 매우 간단하게 이를 수행할 수 있는 `string.upper` 함수를 제공합니다. 이 함수는 모든 알파벳 문자를 대문자로 변환합니다. 특히 Lua에서는 메모리 관리가 중요하기 때문에, 자주 변경되는 문자열에 대해서는 성능에 주의를 기울여야 합니다. 

Python이나 JavaScript와 같은 다른 언어들도 비슷한 기능을 제공하지만, 메소드 이름이 다를 수 있으니 주의하세요(Python: `.upper()`, JavaScript: `.toUpperCase()`). Lua에서는 문자열이 불변 객체이기 때문에, 원본 문자열을 변경하는 것이 아닌 새로운 문자열을 반환하는 것도 차이점입니다.

## See Also
## 관련 참고자료:
- Lua 5.4 Reference Manual: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- Programming in Lua (first edition): [https://www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)
