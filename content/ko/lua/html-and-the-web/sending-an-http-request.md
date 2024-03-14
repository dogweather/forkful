---
date: 2024-01-20 18:00:12.464622-07:00
description: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B8\uB2E4\uB294 \uAC74 \uC6F9 \uC11C\
  \uBC84\uC640 \uB370\uC774\uD130\uB97C \uC8FC\uACE0\uBC1B\uAE30 \uC704\uD574\uC11C\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uC774\uB97C \uC0AC\uC6A9\
  \uD558\uB294 \uC774\uC720\uB294 \uC6F9 API \uD1B5\uC2E0, \uC6F9 \uD398\uC774\uC9C0\
  \ \uB0B4\uC6A9 \uAC00\uC838\uC624\uAE30, \uC6F9 \uAE30\uBC18 \uB370\uC774\uD130\
  \ \uBD84\uC11D \uB4F1 \uB2E4\uC591\uD55C \uC791\uC5C5\uC744 \uC790\uB3D9\uD654\uD558\
  \uAE30 \uC704\uD574\uC11C\uC8E0."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.412678-06:00'
model: gpt-4-1106-preview
summary: "HTTP \uC694\uCCAD\uC744 \uBCF4\uB0B8\uB2E4\uB294 \uAC74 \uC6F9 \uC11C\uBC84\
  \uC640 \uB370\uC774\uD130\uB97C \uC8FC\uACE0\uBC1B\uAE30 \uC704\uD574\uC11C\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uAC00 \uC774\uB97C \uC0AC\uC6A9\uD558\
  \uB294 \uC774\uC720\uB294 \uC6F9 API \uD1B5\uC2E0, \uC6F9 \uD398\uC774\uC9C0 \uB0B4\
  \uC6A9 \uAC00\uC838\uC624\uAE30, \uC6F9 \uAE30\uBC18 \uB370\uC774\uD130 \uBD84\uC11D\
  \ \uB4F1 \uB2E4\uC591\uD55C \uC791\uC5C5\uC744 \uC790\uB3D9\uD654\uD558\uAE30 \uC704\
  \uD574\uC11C\uC8E0."
title: "HTTP \uC694\uCCAD \uBCF4\uB0B4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜 사용하는가?)
HTTP 요청을 보낸다는 건 웹 서버와 데이터를 주고받기 위해서입니다. 프로그래머가 이를 사용하는 이유는 웹 API 통신, 웹 페이지 내용 가져오기, 웹 기반 데이터 분석 등 다양한 작업을 자동화하기 위해서죠.

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
