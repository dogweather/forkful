---
title:                "웹 페이지 다운로드"
html_title:           "Lua: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지 다운로드를 하는 것은 간단히 말하면 인터넷에서 웹 페이지를 받아오는 것입니다. 개발자들은 이것을 하는 이유는 다양합니다. 예를 들어, 웹 크롤링을 통해 정보를 수집하거나, 자동으로 웹 페이지를 다운로드하여 사용자에게 보여줄 수 있습니다.

## 어떻게:

```Lua
-- url을 사용하여 웹 페이지 다운로드하기
local http = require("socket.http")
local body, code, headers = http.request("https://www.example.com")

-- 다운로드한 웹 페이지 내용 출력하기
print(body)

-- 다운로드한 웹 페이지 내용을 파일로 저장하기
local file = io.open("page.html", "w")
file:write(body)
file:close()
```

## 깊이 파보기:

웹 페이지 다운로드는 인터넷 사용이 보편화되기 전부터 존재하던 기술입니다. 초기에는 ftp나 telnet과 같은 프로토콜을 사용하여 다운로드를 진행했지만, 현재는 보다 편리한 http 프로토콜을 사용합니다. 또한, 새로운 기술인 웹 소켓을 사용하여 실시간 웹 페이지 다운로드가 가능합니다.

웹 페이지를 다운로드하는 다른 방법으로는 cURL이나 Python의 requests 모듈을 사용하는 것이 있습니다. 그러나 Lua에는 기본적으로 내장된 http 모듈을 사용하는 것이 가장 간단하고 효율적입니다.

## 참고 자료:

- [Lua 공식 문서](https://www.lua.org/manual/5.4/manual.html)
- [socket.http 모듈 예제](https://nachtimwald.com/2013/09/21/reading-web-pages-in-lua-with-the-built-in-http-libraries/)
- [웹 소켓 사용 예제](https://github.com/tylerneylon/lua_websocket_examples)