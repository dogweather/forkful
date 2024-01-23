---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:02:20.704240-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇 & 왜?)
HTTP 기본 인증을 사용한 요청은 서버에 사용자 아이디와 비밀번호를 보내 접근 권한을 확인합니다. 프로그래머들은 안전한 정보 전송을 위해 이 방식을 사용합니다.

## How to: (방법)
```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

-- 사용자 이름과 비밀번호 설정
local username = "your_username"
local password = "your_password"

-- 기본 인증 헤더 생성
local auth_header = "Basic " .. (username .. ":" .. password):gsub("\n", "")

local response_body = {}

-- HTTP 요청
local res, code, response_headers = http.request{
  url = "http://yourserver.com/data",
  method = "GET",
  headers = {
    ["Authorization"] = auth_header
  },
  sink = ltn12.sink.table(response_body)
}

-- 응답 확인
if code == 200 then
  print(table.concat(response_body))
else
  print("Error: " .. (code or "no response"))
end
```

예상 출력:
```
{"data": "some secure data"}
```

## Deep Dive (심층 탐구)
HTTP 기본 인증은 RFC 7617에 정의된 오래된 프로토콜입니다. 요청 데이터가 암호화되지 않으므로 HTTPS와 함께 사용할 때 더 안전합니다. 인증에 더 안전한 OAuth와 같은 대체 수단들이 존재하며, 예를 들면 API 키를 사용한 방식 등이 있습니다. Lua에서는 `socket.http` 라이브러리를 많이 사용하는데, `lua-requests`나 `LuaSec`와 함께 사용해 SSL/TLS를 지원하는 HTTPS 요청도 가능합니다.

## See Also (참고자료)
- Lua 라이브러리: [LuaSocket](http://w3.impa.br/~diego/software/luasocket/)
- RFC 7617, The 'Basic' HTTP Authentication Scheme: [RFC 7617](https://tools.ietf.org/html/rfc7617)
- LuaSec, a binding for OpenSSL to provide TLS/SSL communication: [LuaSec GitHub](https://github.com/brunoos/luasec)
- 'lua-requests' for more advanced HTTP requests: [lua-requests GitHub](https://github.com/JakobGreen/lua-requests)
