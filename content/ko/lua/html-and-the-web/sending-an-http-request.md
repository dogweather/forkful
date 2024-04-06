---
date: 2024-01-20 18:00:12.464622-07:00
description: "How to: (\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Lua\uC5D0\uC11C HTTP\
  \ \uC694\uCCAD\uC744 \uD558\uB824\uBA74, `socket.http` \uB77C\uC774\uBE0C\uB7EC\uB9AC\
  \uB098 `luasocket` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4. \uBA3C\uC800 `luasocket`\uB97C \uC124\uCE58\uD574\uC57C \uD569\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.102120-06:00'
model: gpt-4-1106-preview
summary: "(\uC5B4\uB5BB\uAC8C \uD558\uB098\uC694?) Lua\uC5D0\uC11C HTTP \uC694\uCCAD\
  \uC744 \uD558\uB824\uBA74, `socket.http` \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB098 `luasocket`\
  \ \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
weight: 44
---

## How to: (어떻게 하나요?)
Lua에서 HTTP 요청을 하려면, `socket.http` 라이브러리나 `luasocket` 모듈을 사용할 수 있습니다. 먼저 `luasocket`를 설치해야 합니다:

```lua
-- Luasocket library 설치
luarocks install luasocket
```

GET 요청 예시:

```lua
local http = require("socket.http")

-- HTTP GET 요청 보내기
local response_body = {}

http.request{
    url = "http://httpbin.org/get",
    sink = ltn12.sink.table(response_body)
}

-- 응답 출력하기
print(table.concat(response_body))
```

POST 요청 예시:

```lua
local http = require("socket.http")
local ltn12 = require("ltn12")

-- 데이터 정의
local request_body = "name=Lua&programming=isFun"
local response_body = {}

-- HTTP POST 요청 보내기
http.request{
    url = "http://httpbin.org/post",
    method = "POST",
    headers = {
        ["Content-Type"] = "application/x-www-form-urlencoded",
        ["Content-Length"] = tostring(#request_body)
    },
    source = ltn12.source.string(request_body),
    sink = ltn12.sink.table(response_body)
}

-- 응답 출력하기
print(table.concat(response_body))
```

## Deep Dive (심화 학습)
과거엔 Lua에 표준 HTTP 라이브러리가 없어 대부분의 HTTP 작업이 외부 라이브러리에 의존했습니다. `luasocket`은 가장 인기 있는 선택지 중 하나입니다. Lua의 버전 5.1부터 사용됩니다. `socket.http`는 단순한 사용을 위해, `copas.http`나 `lua-http` 같은 라이브러리는 좀 더 고급 기능과 비동기 작업을 위해 존재합니다.

## See Also (더 알아보기)
- Luasocket GitHub 페이지: https://github.com/diegonehab/luasocket
- LuaRocks: https://luarocks.org/
- HTTPbin (HTTP 요청 실험): http://httpbin.org/
