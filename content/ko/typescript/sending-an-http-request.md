---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용할까?
HTTP 요청을 보내는 것은 웹 서버에 정보를 요청하거나 전송하는 방법입니다. 프로그래머들이 이를 사용하는 주된 이유는 서버에서 데이터를 가져와 작업하거나, 서버에 데이터를 저장하는 것입니다. 

## 어떻게 사용할까:
TypeScript를 사용하여 HTTP 요청을 보내는 방법을 알아보겠습니다. 먼저, `http` 모듈을 임포트하고, URL을 선택하고 요청(GET, POST 등)을 보냅니다.

```TypeScript
import http from 'http';

const options = {
  hostname: 'www.example.com',
  port: 80,
  path: '/foo.json',
  method: 'GET',
};

const req = http.request(options, res => {
  res.on('data', (d) => {
    process.stdout.write(d);
  });
});
```
이 코드는 `www.example.com/foo.json` 위치에서 JSON 데이터를 요청합니다. 응답이 도착하면, 데이터 `d`가 콘솔에 출력됩니다.

## 깊게 알아보기
HTTP 요청은 프로그래밍의 핵심 요소로, 웹의 기초적인 통신 메커니즘입니다. 이는 1990년대 초 웹의 탄생과 함께 개발되었습니다. 

대체 방안으로는 WebSocket, GraphQL 등 다양한 기술들이 존재하나, 간단한 정보 요청 또는 전송에는 HTTP가 가장 흔히 사용됩니다. 

TypeScript에서 HTTP 요청을 보내기 위해, Node.js의 `http` 모듈을 사용합니다. 이 모듈은 내부적으로 TCP/IP 소켓을 이용해 통신하며, 요청 헤더와 본문을 조작할 수 있는 API를 제공합니다.

## 참고 자료
1. [MDN - HTTP 가이드](https://developer.mozilla.org/ko/docs/Web/HTTP)
2. [Node.js - `http` 모듈 문서](https://nodejs.dev/learn/making-http-requests-with-nodejs)
3. [GraphQL 공식 홈페이지](https://graphql.org/)
4. [WebSocket API - 웹소켓 개요](https://developer.mozilla.org/ko/docs/Web/API/WebSockets_API/Writing_WebSocket_servers)