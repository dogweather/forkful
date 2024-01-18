---
title:                "HTTP 요청 보내기"
html_title:           "Lua: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 무엇이고 왜?

HTTP 요청을 보내는 것은 인터넷에서 데이터를 가져오기 위한 프로그래머의 일반적인 방법입니다. 이를 통해 웹 사이트, API 또는 다른 온라인 서비스로부터 정보를 받아올 수 있습니다. 

## 하는 법:

```
-- 기본적인 GET 요청 예제
local http = require("socket.http") 
local response = http.request("http://www.example.com") 
print(response)  -- 웹사이트의 HTML 코드를 출력
```

```
-- 매개변수를 포함하는 GET 요청 예제
local http = require("socket.http") 
local ltn12 = require("ltn12") -- ltn12 모듈을 불러옴
local params = {q = "Lua programming"} -- 요청의 매개변수 설정
local response = {} -- 요청 결과를 저장할 테이블
local result, code, headers = http.request{
    url = "http://www.google.com/search", 
    method = "GET", 
    sink = ltn12.sink.table(response),
    headers = {
        ["User-Agent"] = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:64.0) Gecko/20100101 Firefox/64.0" 
        -- 구글의 요청을 흉내내기 위해 user-agent를 설정
    }, 
    params = params 
    -- 설정한 매개변수를 요청에 포함
}

if result then
    print(response[1])  -- 맨 첫 줄의 검색 결과 출력
else
    print("Error: "..code) -- 에러 발생 시 에러 코드 출력
end
```

## 깊이 파고들기:

이전에는 HTTP 요청을 보내기 위해 라이브러리를 사용해야 했지만 Lua 5.1부터는 소켓 모듈이 기본으로 포함되어 있어서 추가적인 라이브러리 없이도 HTTP 요청을 보낼 수 있게 되었습니다. 하지만 라이브러리를 사용하면 더 간편하고 안전하게 HTTP 요청을 할 수 있습니다.

대체로 마이크로서비스와 웹 API의 발전과 함께, HTTP 요청은 프로그래머에게 매우 중요한 기술이 됐습니다. 이를 통해 다른 웹 사이트나 앱과 정보를 교환하거나 특정 웹 페이지에서 데이터를 가져오는 등 다양한 기능을 구현할 수 있습니다.

## 추가 자료:

- Lua 소켓 모듈 문서 : http://w3.impa.br/~diego/software/luasocket/index.html
- Lua 소켓 모듈 예제 : https://github.com/diegonehab/luasocket/blob/master/samples/http.lua
- HTTP 요청과 다른 네트워크 기능을 구현하는 라이브러리들 : https://github.com/luaforge/lua-protocol-http/tree/master/lib