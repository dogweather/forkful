---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Bash: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

HTTP 요청을 기본 인증과 함께 보내는 것이란, 클라이언트와 서버의 의사소통에 대한 기본적인 보안 접근법입니다. 프로그래머들이 이를 사용하여, 특히 웹 서비스에 대한 요청을 안전하게 전송하고, 사용자 인증을 수행합니다.

## 방법:

TypeScript에서 HTTP 요청을 보내는 데는 `axios`라는 모듈을 사용할 수 있습니다. 이는 프로미스를 참조하여 비동기적 작업을 처리하며, 브라우저와 node.js 모두에서 작동하는 기능을 가지고 있습니다.

```TypeScript
import axios from 'axios';

const sendRequest = async () => {
  let username = 'my-username';
  let password = 'my-password';
  
  try {
    const response = await axios.get('http://my-api-url.com', {
      auth: {
        username: username,
        password: password
      }
    });
    console.log(response.data);
  } catch (error) {
    console.error(error);
  }
};

sendRequest();
```

이 코드는 `http://my-api-url.com`에 GET 요청을 보내는 예시로, 기본 인증을 포함하고 있습니다. 요청이 성공하면 응답 데이터가 콘솔에 로깅됩니다. 

## 깊이 파헤쳐보기:

HTTP 기본 인증은 웹의 초기 시대부터 있던, 매우 간단한 인증 방식입니다. secure, digest, NTLM, form-based 와 같게 웹 인증 분야에서 다양한 대체 방법들이 존재하긴 하지만, 기본 인증은 매우 간단하고 가볍기에 여전히 널리 사용됩니다.

그러나, 중요한 정보의 데이터가 노출될 위험 때문에, SSL/TLS와 같은 암호화된 연결 위에서만 사용하는 것이 안전합니다.

## 추가로 참고할 만한 것:

1. [MDN Web Docs: HTTP 인증](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)
2. [Axios GitHub Repository](https://github.com/axios/axios)
3. [Stack Overflow Threads about Basic Authentication](https://stackoverflow.com/questions/tagged/basic-authentication?tab=Votes)
4. [NPM Axios Documentation](https://axios-http.com/docs/intro)