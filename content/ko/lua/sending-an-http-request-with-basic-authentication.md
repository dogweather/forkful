---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 기본 인증으로 보내는 것은 웹 서비스와 통신하는 방법입니다. 프로그래머들이 이 작업을 수행하는 이유는 사용자 이름과 비밀번호를 이용해 웹 서비스에 접근하기 위해서입니다.

## 어떻게:

아래에 Lua 코드 예제를 주었습니다.

```Lua
local http = require("socket.http")
local ltn12 = require("ltn12")

local url = 'http://example.com'
local user = 'username'
local password = 'password'
local response = {}

http.request{
  url = url,
  user = user,
  password = password,
  sink = ltn12.sink.table(response)}

print(table.concat(response))
```

## 깊게 들어가보기:

서버에 대한 HTTP 요청을 보내는 방법은 여러 가지가 있지만, 기본 인증을 이용한 방법은 가장 간단하고 일반적인 방법 중 하나입니다. 이 방법은 웹이 처음 개발되었을 때부터 있었으며, 사용자가 자신을 인증하고 서버에 대한 접근 권한을 갖게하는 주요 방법 중 하나였습니다.

하지만 오늘날에는 기본 인증 방식보다 더 많은 보안기능을 갖춘 방법들, 예를 들어 OAuth나 JWT 등이 많이 사용되고 있습니다. 이 알고리즘이 더 복잡하긴 하지만, 더 나은 보안성을 제공합니다.

## 참고 자료:

* [Lua 소켓 라이브러리 문서](http://w3.impa.br/~diego/software/luasocket/http.html)
* [HTTP Basic Authentication에 대한 위키 백과의 설명](https://ko.wikipedia.org/wiki/HTTP_%EA%B8%B0%EB%B3%B8_%EC%9D%B8%EC%A6%9D)
* [새로운 인증 방식에 대한 명세: OAuth](https://oauth.net/2/)