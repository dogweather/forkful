---
title:                "HTML 파싱"
date:                  2024-01-20T15:32:51.859751-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇이며 왜하는가?
HTML 파싱은 HTML 문서에서 정보를 추출하는 과정입니다. 프로그래머들은 데이터를 다루거나 웹 스크래핑할 때 이 작업을 합니다.

## 실행 방법:
Lua에서 HTML을 파싱하기 위해 `lxsh` 모듈을 사용할 수 있습니다. 아래는 간단한 예제입니다:

```Lua
local lxsh = require 'lxsh'

-- HTML 문자열
local html = [[
<html>
<head>
    <title>Test Page</title>
</head>
<body>
    <h1>Hello, Lua!</h1>
    <p>This is a paragraph.</p>
</body>
</html>
]]

-- lxsh를 사용한 파싱
local parser = lxsh.parse.html()
for kind, text in parser:match(html) do
  if kind == 'start-tag' then
    print('Start tag:', text)
  elseif kind == 'end-tag' then
    print('End tag:', text)
  elseif kind == 'text' then
    print('Text:', text)
  end
end
```

## 심층 탐구:
HTML 파싱은 웹의 초창기부터 필요했습니다. 초기에는 정규식을 많이 사용했지만, 정확하지 않고 복잡한 HTML에는 적합하지 않았습니다. Lua에서 HTML 파싱을 위한 대안으로는 `luaxml`이나 `htmlparser` 라이브러리도 있습니다. `lxsh`는 구문 분석과 태그의 계층적 관계를 처리하는데 더 직관적입니다. 구현 세부 사항에서는 퍼포먼스 최적화를 위해 C 라이브러리를 바인딩하기도 합니다.

## 참고 자료:
- lxsh GitHub 페이지: https://github.com/daurnimator/lxsh
- Lua HTML parser GitHub 페이지: https://github.com/msva/lua-htmlparser
- LuaXML 공식 매뉴얼: http://www.keplerproject.org/luaxml/