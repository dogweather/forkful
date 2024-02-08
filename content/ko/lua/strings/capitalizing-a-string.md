---
title:                "문자열 대문자화"
date:                  2024-02-03T19:05:59.996446-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜인가?
문자열의 첫 글자를 대문자로 변경하고 나머지는 소문자로 유지하여 문자열의 첫 글자를 대문자화하는 것을 말합니다. 이 기술은 텍스트 형식을 더 전문적이거나 읽기 쉬운 출력물로 준비하는 데 자주 사용됩니다. 예를 들어 제목이나 사용자 입력을 표시 준비에 사용됩니다.

## 어떻게 하나:
Lua에는 문자열을 대문자화하는 내장 함수가 없지만, 기본 문자열 조작 함수를 사용하여 이 작업을 쉽게 수행할 수 있습니다. 여기 단일 단어의 첫 글자를 대문자화하는 간단한 함수가 있습니다:

```lua
function capitalize(word)
    return word:sub(1,1):upper() .. word:sub(2):lower()
end

print(capitalize("hello"))  -- 출력: Hello
```

문장의 각 단어를 대문자화하려면, 문장을 단어로 분할하고 각각을 대문자화한 다음 다시 합치면 됩니다:

```lua
function capitalizeSentence(sentence)
    local words = {}
    for word in sentence:gmatch("%S+") do
        table.insert(words, capitalize(word))
    end
    return table.concat(words, " ")
end

print(capitalizeSentence("hello world from lua"))  -- 출력: Hello World From Lua
```

성능이 중요한 프로젝트를 진행 중이고 더 고급 문자열 조작 기능이 필요하다면, `Penlight`와 같은 타사 라이브러리를 사용하는 것을 고려해보십시오. Penlight는 다양한 문자열 처리 기능뿐만 아니라 다른 유틸리티를 포함하여 Lua를 개선합니다:

```lua
-- Penlight가 설치되어 있다고 가정:
local pl = require("pl.stringx")
local text = "hello lua users"
text = pl.capitalized(text)
print(text)  -- 출력: Hello lua users

-- 참고: Penlight의 capitalized 함수는 첫 단어만 대문자화합니다.
-- 각 단어를 대문자화하기 위해서는 여전히 사용자 정의 솔루션을 구현하거나 다른 라이브러리를 살펴봐야 합니다.
```
