---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:48.132159-07:00
description: "\uBC29\uBC95: Lua\uC5D0\uB294 HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\
  \uD55C \uB0B4\uC7A5 \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC5C6\uC9C0\uB9CC, `LuaHTML`\uC774\
  \uB098 `LuaXML`\uC744 \uD1B5\uD55C `libxml2` \uBC14\uC778\uB529\uC744 \uD65C\uC6A9\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\
  \uD55C \uC778\uAE30 \uC788\uB294 \uC811\uADFC \uBC29\uBC95\uC740 HTML5 \uADDC\uACA9\
  \uC744 \uC900\uC218\uD558\uB294 \uAC04\uB2E8\uD558\uBA74\uC11C\uB3C4 \uC9C1\uAD00\
  \uC801\uC778 \uD30C\uC2F1 \uAE30\uB2A5\uC744\u2026"
lastmod: '2024-03-13T22:44:55.414166-06:00'
model: gpt-4-0125-preview
summary: "Lua\uC5D0\uB294 HTML\uC744 \uD30C\uC2F1\uD558\uAE30 \uC704\uD55C \uB0B4\uC7A5\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uAC00 \uC5C6\uC9C0\uB9CC, `LuaHTML`\uC774\uB098\
  \ `LuaXML`\uC744 \uD1B5\uD55C `libxml2` \uBC14\uC778\uB529\uC744 \uD65C\uC6A9\uD560\
  \ \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 방법:
Lua에는 HTML을 파싱하기 위한 내장 라이브러리가 없지만, `LuaHTML`이나 `LuaXML`을 통한 `libxml2` 바인딩을 활용할 수 있습니다. HTML을 파싱하기 위한 인기 있는 접근 방법은 HTML5 규격을 준수하는 간단하면서도 직관적인 파싱 기능을 제공하는 `lua-gumbo` 라이브러리를 사용하는 것입니다.

### lua-gumbo 설치하기:
먼저, `lua-gumbo`가 설치되어 있는지 확인하세요. 일반적으로 luarocks를 사용하여 설치할 수 있습니다:

```sh
luarocks install lua-gumbo
```

### lua-gumbo를 사용한 기본 파싱:
`lua-gumbo`를 사용하여 간단한 HTML 스니펫을 파싱하고 데이터를 추출하는 방법은 다음과 같습니다:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>안녕하세요, 세계!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- 출력: 안녕하세요, 세계!
```

### 고급 예제 - 링크 추출하기:
HTML 문서에서 모든 앵커 태그(`<a>` 엘리먼트)의 `href` 속성을 추출하기 위해서:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>샘플 페이지</title></head>
<body>
  <a href="http://example.com/1">링크 1</a>
  <a href="http://example.com/2">링크 2</a>
  <a href="http://example.com/3">링크 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- 이것이 엘리먼트이고 속성이 있는지 확인
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- 샘플 출력:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

이 코드 스니펫은 문서의 모든 링크를 순회하며 그들의 `href` 속성을 출력합니다. `lua-gumbo` 라이브러리는 HTML 문서의 구조를 파싱하고 이해하는 능력을 통해 태그나 속성에 기반한 특정 요소를 추출하는 과정을 단순화합니다.
