---
title:                "기본 인증을 이용한 HTTP 요청 보내기"
html_title:           "Arduino: 기본 인증을 이용한 HTTP 요청 보내기"
simple_title:         "기본 인증을 이용한 HTTP 요청 보내기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

HTTP 요청을 기본 인증과 함께 보내는 것은 사용자 이름과 비밀번호를 포함하는 요청을 보내는 방법입니다. 개발자들이 이를 사용하는 이유는 서버로부터 보호된 리소스를 안전하게 요청하기 위해서입니다.

## 어떻게 사용하나요:

다음 예시는 자바스크립트로 기본 인증을 사용하여 HTTP 요청을 보내는 방법을 보여줍니다. 

```Javascript 
const axios = require('axios');

axios({
  method: 'get',
  url: 'http://example.com',
  auth: {
    username: 'username',
    password: 'password'
  }
})
.then((response) => {
  console.log(response.data);
})
.catch((error) => {
  console.error(error);
});
```

이 예제에서, auth 객체는 사용자 이름과 비밀번호를 포함하며, axios는 이를 사용하여 HTTP 요청을 보냅니다.

## 깊게 알아보기:

기본 인증을 사용하여 HTTP 요청을 보내는 것은 웹의 초기 시절부터 사용해온 방법입니다. 그러나 이 방법은 비밀번호를 본문에 포함하므로, 해당 요청이 전송 중에 중간에서 가로채일 수 있다는 단점이 있습니다. 

또한, 대안으로서 다른 인증 메커니즘, 예를 들어 OAuth, Bearer Token, JWT 등도 종종 사용됩니다. 

구현에 있어서는, 기본 인증을 사용하는 경우, 사용자 이름과 비밀번호는 Base64 형식으로 인코딩되어 'Authorization' 헤더에 포함됩니다. 이는 암호화가 아닌, 단순히 텍스트 형식의 변환에 불과하므로 주의가 필요합니다.

## 참고 자료:

Btoa() 함수에 대한 자세한 내용은 MDN 웹 문서를 참조하십시오: [MDN Web Docs - btoa()](https://developer.mozilla.org/ko/docs/Web/API/WindowOrWorkerGlobalScope/btoa)

Axios와 관련한 자세한 정보는 공식 문서를 참조하십시오: [Axios Documentation](https://axios-http.com/docs/intro)

또한 어떤 인증 메커니즘이 가장 적합한지 결정하는데 도움이 될 수 있는 IETF의 인증 체계에 대한 문서도 참조하십시오: [IETF - HTTP Authentication](https://datatracker.ietf.org/doc/html/rfc2617)