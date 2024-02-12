---
title:                "HTML 파싱"
aliases:
- /ko/lua/parsing-html/
date:                  2024-02-03T19:12:48.132159-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML 파싱"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTML 파싱은 HTML 문서로부터 데이터와 정보를 추출하는 것을 말하며, 웹 스크래핑, 데이터 분석, 자동화 작업에 있어 필수적입니다. 프로그래머들은 프로그래매틱하게 웹 콘텐츠를 수집, 분석 또는 조작하기 위해 이 작업을 수행하며, 이는 웹사이트에서 데이터를 수동으로 추출하는 것을 자동화할 수 있게 해줍니다.

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
