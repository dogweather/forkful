---
title:                "Javascript: 기본 인증을 사용한 http 요청 보내기"
simple_title:         "기본 인증을 사용한 http 요청 보내기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

기본 인증을 사용하여 HTTP 요청을 보내는 이유는 서버와의 통신에서 인증된 사용자를 식별하고 보안을 유지하기 위해서입니다.

## 어떻게

```Javascript
const request = require('request');

const options = {
    url: 'https://www.example.com',
    headers: {
        'Authorization': 'Basic ' + Buffer.from('username:password').toString('base64')
    }
}

request(options, (error, response, body) => {
    if (error) throw new Error(error);

    console.log(body);
});

```

```
<html>
    <h1>여기는 보안이 필요한 페이지입니다.</h1>
    <p>사용자의 인증 정보를 입력해주세요.</p>
</html>
```

## 딥 다이브

HTTP 기본 인증은 클라이언트가 요청을 보낼 때 사용자 이름과 비밀번호를 Base64 인코딩하여 서버로 전송하는 인증 방식입니다. 이는 보안성이 낮으며, 인증 헤더가 평문으로 전송되므로 보안에 취약할 수 있습니다. 따라서 HTTPS와 같은 보안 프로토콜을 사용하는 것이 좋습니다. 또한 서버는 클라이언트가 전송한 인증 정보를 확인하여 서비스를 제공합니다.

## 참고 자료

- HTTP 기본 인증에 관한 MDN 문서: https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication#Basic_authentication_scheme
- Node.js에서 HTTP 요청 보내는 방법: https://nodejs.org/api/http.html#http_http_request_url_options_callback
- Base64 인코딩에 관한 MDN 문서: https://developer.mozilla.org/ko/docs/Web/API/WindowBase64/Base64_encoding_and_decoding