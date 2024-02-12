---
title:                "기본 인증을 사용한 HTTP 요청 보내기"
date:                  2024-01-20T18:02:57.331396-07:00
model:                 gpt-4-1106-preview
simple_title:         "기본 인증을 사용한 HTTP 요청 보내기"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
HTTP 요청에 기본 인증(Basic Authentication)을 붙이는 것은 사용자 이름과 비밀번호를 포함해 서버에 접근하게 합니다. 프로그래머들이 이를 사용하는 이유는 간단하게 API나 리소스에 보안을 요구할 때입니다.

## How to: (방법)
```TypeScript
import axios from 'axios';

async function getWithBasicAuth(url: string, username: string, password: string) {
  try {
    const response = await axios.get(url, {
      auth: {
        username: username,
        password: password,
      },
    });
    console.log(response.data);
  } catch (error) {
    console.error('Authentication failed', error);
  }
}

// 사용 예
const apiURL = 'https://api.example.com/data';
const username = 'user1';
const password = 'pass1234';

getWithBasicAuth(apiURL, username, password);
```
성공적인 요청은 `console.log(response.data);`에서 데이터를 출력할 것입니다.

## Deep Dive (심층 분석)
기본 인증은 HTTP 1.0 부터 있었고 가장 초기의 인증 방식 중 하나입니다. Base64로 인코딩된 ‘username:password’를 HTTP 헤더에 실어 보내는 방식이죠. 하지만 이 방식은 보안에 취약하여 HTTPS와 함께 사용되어야 합니다.

인증의 대안들로는 OAuth, API 키, JWT(Json Web Tokens) 등이 있습니다. 이러한 방식들은 더 안전하고 유연하지만 기본 인증은 여전히 간단히 사용할 수 있고 빠르게 세팅할 수 있다는 장점이 있습니다.

구현 시에는 `Authorization` 헤더에 값을 넣는 것을 잊지 말아야 하며, 앞서 본 예제에서는 Axios 라이브러리를 이용해 처리합니다. Axios는 이러한 방식을 내부적으로 `Authorization` 헤더에 알맞게 설정해줍니다.

## See Also (더 보기)
- Axios GitHub repository: https://github.com/axios/axios
- MDN Web Docs on Basic authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
