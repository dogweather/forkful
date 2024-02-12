---
title:                "웹 페이지 다운로드하기"
aliases:
- /ko/lua/downloading-a-web-page.md
date:                  2024-01-20T17:44:35.685636-07:00
model:                 gpt-4-1106-preview
simple_title:         "웹 페이지 다운로드하기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
웹 페이지를 다운로드하는 것은 인터넷상의 문서를 로컬 컴퓨터로 가져오는 일입니다. 프로그래머들은 데이터 수집, 자동화 및 백업을 위해 이 작업을 수행합니다.

## How to: (방법:)
Lua에서 웹 페이지를 다운로드할 때, `socket.http` 라이브러리를 사용합니다. 아래는 간단한 예시 코드입니다.

```Lua
local http = require("socket.http")

-- 웹 페이지의 URL을 지정합니다.
local url = "http://example.com"

-- URL로부터 데이터를 가져옵니다.
local body, code, headers, status = http.request(url)

-- 응답 내용을 출력합니다.
print(body) -- 웹 페이지의 HTML 내용
print(code) -- HTTP 상태 코드
```

실행 결과:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
...
</html>

200
```

## Deep Dive (심층 탐구):
과거에 Lua에서 웹 페이지를 다운로드하려면 외부 프로세스를 호출하거나 복잡한 소켓 처리를 직접 구현해야 했습니다. `socket.http`는 LuaSocket 라이브러리의 일부로, Lua에서 HTTP 프로토콜을 간편하게 사용하게 해 줍니다. HTTPS 웹 페이지를 다운로드하려면 `ssl.https` 모듈을 쓰는 것이 좋습니다. 모듈을 사용하면 추가 보안을 제공하는 SSL/TLS 프로토콜을 사용할 수 있기 때문입니다.

다른 방법으로는, `wget`이나 `curl`과 같은 커맨드라인 툴을 os.execute()를 통해 호출하는 방식이 있습니다. 이는 더 복잡한 기능이나 섬세한 설정이 필요할 때 유용하게 사용될 수 있습니다.

## See Also (관련 링크):
- LuaSocket 공식 문서: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec, HTTPS를 위한 Lua 모듈: https://github.com/brunoos/luasec
- `wget`: https://www.gnu.org/software/wget/
- `curl`: https://curl.se/
