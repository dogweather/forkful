---
title:                "TypeScript: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청에 기본 인증을 추가하는 것을 이유는 데이터를 보호하기 위해서 입니다. 기본 인증은 사용자 이름과 비밀번호를 사용하여 요청을 보낼 때, 서버가 사용자를 식별하고 인증하는 간단한 방법입니다.

## 사용 방법

```TypeScript
import * as https from 'https';

// 기본 인증 정보 설정
const auth = 'username:password';
// 인코딩하여 문자열 생성
const encodedAuth = Buffer.from(auth).toString('base64');
// 요청 헤더에 추가
const options = {
    headers: {
        'Authorization': `Basic ${encodedAuth}`,
    }
};

const request = https.request('https://example.com/', options, (res) => {
    res.on('data', (chunk) => {
        console.log(`Received data: ${chunk}`);
    });
});

request.on('error', (error) => {
    console.error(`Error sending request: ${error.message}`);
});

request.end();
```

위의 코드는 Node.js와 https 모듈을 사용하여 기본 인증을 포함한 HTTP 요청을 보내는 예제입니다. 인증 정보를 인코딩하여 요청 헤더에 추가한 후, 주어진 URL로 요청을 보냅니다.

예상 출력:

```
Received data: <html>
<body>
<h1>Hello World!</h1>
</body>
</html>
```

## 깊이 파헤치기

HTTP 요청에 기본 인증을 추가하는 과정에 대해 더 자세히 알아보겠습니다. 먼저, 인증 정보를 인코딩하여 문자열을 생성해야 합니다. 이는 Base64 인코딩을 사용하여 사용자 이름과 비밀번호를 암호화하고 요청 헤더에 추가하는 것을 의미합니다. 이렇게 함으로써, 요청을 받은 서버는 사용자를 식별하고 인증할 수 있게 됩니다.

또한, 기본 인증은 매우 간단한 보안 방식이기 때문에 보안에 취약할 수 있습니다. 따라서 민감한 정보를 전송하는 경우에는 추가적인 보안 방식을 사용하는 것이 좋습니다.

## 참고

- [Node.js Documentation on Basic Authentication](https://nodejs.org/api/http.html#http_http_request_options_callback)
- [MDN Web Docs on Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Base64 Encoding Tutorial](https://www.base64encode.org/)