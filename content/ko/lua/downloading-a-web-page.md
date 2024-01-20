---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜하며 & 왜?

웹 페이지 다운로드는 사용자가 웹 사이트의 콘텐츠를 로컬 시스템으로 복사하는 것을 의미합니다. 프로그래머들은 이를 통해 웹사이트 데이터의 백업, 정보 수집, 웹 스크레이핑 등을 수행 할 수 있습니다.

## 구현 방법:

Lua에서 웹페이지를 다운로드하는 방법은 다음과 같습니다.

```Lua
-- LuaSocket은 Lua에서 HTTP 요청을 하기위한 라이브러리
local http = require("socket.http")

-- 웹페이지 URL
local url = "http://example.com"

-- HTTP 요청
local body, code = http.request(url)

if code == 200 then
    -- 성공적인 요청이면, 웹페이지의 내용을 출력
    print(body)
else
    -- 요청 실패
    print("HTTP 에러: ".. code)
end
```
코드 실행 결과는 웹 페이지의 HTML 소스나 오류 메시지가 될 것입니다.

## 더 깊이 배워보기

- **역사**: LuaSocket 라이브러리는 Lua로 네트워크와 통신하게 해주는 중요한 라이브러리입니다. 이 라이브러리는 Lua 5.1 버전부터 사용 가능하게 되었습니다.
- **대체 방법**: 물론, Lua 이외에도 Python의 BeautifulSoup 라이브러리나 Ruby의 Nokogiri 라이브러리, Node.js의 axios 패키지 등 웹페이지 다운로드를 위한 대다수의 프로그래밍 언어들이 서로 다른 라이브러리를 제공합니다.
- **구현 세부 정보**: `http.request(url)` 함수는 GET 요청을 수행하고, 응답 본문과 HTTP 상태 코드를 반환합니다. 실패 시 nil과 함께 오류 메시지를 반환합니다.

## 관련 자료

- LuaSocket 매뉴얼: http://w3.impa.br/~diego/software/luasocket/http.html
- Lua Programming: https://www.lua.org/start.html
- HTTP 상태 코드: https://developer.mozilla.org/ko/docs/Web/HTTP/Status