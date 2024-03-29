---
date: 2024-01-20 17:58:30.654724-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9\uACFC \uCE58\uD658\uC740 \uBB38\uC790\
  \uC5F4\uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744 \uCC3E\uC544\
  \ \uB2E4\uB978 \uAC83\uC73C\uB85C \uBC14\uAFB8\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC790\uB3D9\uD654\uB41C \uC218\uC815\
  , \uB370\uC774\uD130 \uC815\uC81C, \uD639\uC740 \uC124\uC815 \uBCC0\uACBD \uB4F1\
  \uC744 \uC704\uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.396643-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9\uACFC \uCE58\uD658\uC740 \uBB38\uC790\uC5F4\
  \uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744 \uCC3E\uC544 \uB2E4\
  \uB978 \uAC83\uC73C\uB85C \uBC14\uAFB8\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC790\uB3D9\uD654\uB41C \uC218\uC815, \uB370\
  \uC774\uD130 \uC815\uC81C, \uD639\uC740 \uC124\uC815 \uBCC0\uACBD \uB4F1\uC744 \uC704\
  \uD574 \uC774 \uAE30\uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 검색과 치환은 문자열에서 특정 단어나 패턴을 찾아 다른 것으로 바꾸는 작업입니다. 프로그래머들은 자동화된 수정, 데이터 정제, 혹은 설정 변경 등을 위해 이 기능을 사용합니다.

## How to: (어떻게 하나요?)
Lua에서 문자열을 검색하고 치환하는 기본적인 방법을 아래에 예시로 보여드립니다.

```Lua
local text = "Hello World"
local searchText = "World"
local replaceText = "Lua"

local result = text:gsub(searchText, replaceText)
print(result)  -- 출력: Hello Lua
```

여러 번 치환하는 예시도 있습니다.

```Lua
local text = "반갑습니다. Lua가 무척 반갑습니다."
local searchText = "반갑습니다"
local replaceText = "안녕하세요"

local result = text:gsub(searchText, replaceText)
print(result)  -- 출력: 안녕하세요. Lua가 무척 안녕하세요.
```

## Deep Dive (심층 분석)
Lua에서 문자열 검색과 치환은 주로 `string.find`와 `string.gsub` 함수를 통해 수행됩니다. `string.find`는 패턴이 처음 발견되는 위치를 반환합니다. 반면 `string.gsub`는 모든 일치하는 패턴을 치환하고 치환된 총 개수도 함께 반환합니다.

역사적으로 보면, 문자열 처리는 프로그래밍에서 오랫동안 중요한 역할을 해왔으며, Lua는 이를 위한 강력한 내장 함수들을 제공합니다.

대안으로 정규 표현식을 사용할 수 있는 다른 언어들과는 달리, Lua는 자체 패턴 매칭 시스템을 사용합니다. 이는 완벽한 정규 표현식은 아니지만 대부분의 일반적인 상황에서 충분합니다.

구현 세부사항에서, `string.gsub` 사용 시 성능 고려 사항이 있습니다. 대량의 데이터를 다룰 때는 치환 작업이 성능에 큰 영향을 줄 수 있어, 필요시 다른 방법들을 고려해야 합니다.

## See Also (관련 자료)
Lua의 문자열 처리에 더 깊이 알고 싶다면, 다음 자료들을 참고하시기 바랍니다.

- [Lua 5.4 Reference Manual: string library](https://www.lua.org/manual/5.4/manual.html#6.4)
- [Programming in Lua: Patterns](https://www.lua.org/pil/20.2.html)
