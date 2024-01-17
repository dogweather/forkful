---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "TypeScript: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜: 
HTTP 요청을 기초 인증과 함께 보내는 것은 서버와 클라이언트 간의 암호화된 데이터 통신을 위해 사용됩니다. 프로그래머들은 이를 통해 더 안전한 데이터 교환을 할 수 있습니다.

## 방법: 
```TypeScript
import axios from 'axios';

const username = 'username';
const password = 'password';
const url = 'https://example.com/api';

axios.get(url, {
  auth: {
    username: username,
    password: password
  }
})
.then(response => console.log(response.data))
.catch(error => console.log(error));
```

위의 코드를 실행하면 인증 정보를 포함한 GET 요청을 보내고 응답 데이터를 콘솔에 출력합니다.

## 깊게 파고들기:
기초 인증은 HTTP 프로토콜의 일부로 RFC 2617에 정의되어 있습니다. 이는 인증 정보를 암호화하지 않고 클라이언트에서 서버로 전송하기 때문에 보안 측면에서는 취약할 수 있습니다. 대안으로는 보안 속성을 가지고 있는 OAuth나 OpenID Connect을 사용할 수 있습니다.

서버 측에서는 인증 정보를 검증하기 위해 Base64로 인코딩된 사용자 이름과 비밀번호를 해독하고, 이를 사용하여 사용자를 인증합니다. 인증이 성공적으로 이루어지면 서버는 클라이언트에게 자원에 대한 액세스 권한을 주고, 그렇지 않을 경우 401 Unauthorized 오류를 반환합니다.

## 관련 자료:
- https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- https://www.ietf.org/rfc/rfc2617.txt