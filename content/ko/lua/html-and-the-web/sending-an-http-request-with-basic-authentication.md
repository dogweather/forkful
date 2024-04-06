---
date: 2024-01-20 18:02:20.704240-07:00
description: "How to: (\uBC29\uBC95) HTTP \uAE30\uBCF8 \uC778\uC99D\uC740 RFC 7617\uC5D0\
  \ \uC815\uC758\uB41C \uC624\uB798\uB41C \uD504\uB85C\uD1A0\uCF5C\uC785\uB2C8\uB2E4\
  . \uC694\uCCAD \uB370\uC774\uD130\uAC00 \uC554\uD638\uD654\uB418\uC9C0 \uC54A\uC73C\
  \uBBC0\uB85C HTTPS\uC640 \uD568\uAED8 \uC0AC\uC6A9\uD560 \uB54C \uB354 \uC548\uC804\
  \uD569\uB2C8\uB2E4. \uC778\uC99D\uC5D0 \uB354 \uC548\uC804\uD55C OAuth\uC640 \uAC19\
  \uC740 \uB300\uCCB4 \uC218\uB2E8\uB4E4\uC774 \uC874\uC7AC\uD558\uBA70, \uC608\uB97C\
  \ \uB4E4\uBA74 API \uD0A4\uB97C \uC0AC\uC6A9\uD55C \uBC29\uC2DD \uB4F1\uC774\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.105498-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) HTTP \uAE30\uBCF8 \uC778\uC99D\uC740 RFC 7617\uC5D0 \uC815\
  \uC758\uB41C \uC624\uB798\uB41C \uD504\uB85C\uD1A0\uCF5C\uC785\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
weight: 45
---

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
