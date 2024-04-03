---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:59.996446-07:00
description: "\uC5B4\uB5BB\uAC8C \uD558\uB098: Lua\uC5D0\uB294 \uBB38\uC790\uC5F4\uC744\
  \ \uB300\uBB38\uC790\uD654\uD558\uB294 \uB0B4\uC7A5 \uD568\uC218\uAC00 \uC5C6\uC9C0\
  \uB9CC, \uAE30\uBCF8 \uBB38\uC790\uC5F4 \uC870\uC791 \uD568\uC218\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uC774 \uC791\uC5C5\uC744 \uC27D\uAC8C \uC218\uD589\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uC5EC\uAE30 \uB2E8\uC77C \uB2E8\uC5B4\uC758 \uCCAB \uAE00\uC790\
  \uB97C \uB300\uBB38\uC790\uD654\uD558\uB294 \uAC04\uB2E8\uD55C \uD568\uC218\uAC00\
  \ \uC788\uC2B5\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.394074-06:00'
model: gpt-4-0125-preview
summary: "Lua\uC5D0\uB294 \uBB38\uC790\uC5F4\uC744 \uB300\uBB38\uC790\uD654\uD558\uB294\
  \ \uB0B4\uC7A5 \uD568\uC218\uAC00 \uC5C6\uC9C0\uB9CC, \uAE30\uBCF8 \uBB38\uC790\uC5F4\
  \ \uC870\uC791 \uD568\uC218\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774 \uC791\uC5C5\uC744\
  \ \uC27D\uAC8C \uC218\uD589\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

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
