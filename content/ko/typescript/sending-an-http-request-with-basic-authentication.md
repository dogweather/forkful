---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "TypeScript: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 왜
 HTTP 요청을 기본 인증과 함께 보내는 것이 왜 중요할까요? 기본 인증은 사용자명과 비밀번호를 사용하여 보안을 강화하는 간단하고 효과적인 방법입니다. 따라서 중요한 정보를 전송하거나 가져오는 경우에는 항상 기본 인증을 통해 HTTP 요청을 보내는 것이 좋습니다.

## 방법
```TypeScript
const axios = require('axios');

// 기본 인증을 위한 사용자명과 비밀번호 설정
const username = 'example_username';
const password = 'example_password';

// 요청을 보내기 전에 인증을 설정
axios.defaults.auth = {
  username: username,
  password: password
}

// 기본 인증과 함께 GET 요청 보내기
axios.get('http://www.example.com/api/', {
  auth: axios.defaults.auth
})
  .then(function (response) {
    console.log(response.data); // 예상 출력: 요청한 API의 데이터
  })
  .catch(function (error) {
    console.log(error); // 에러가 발생한 경우 출력
  });
```

## 딥 다이브
기본 인증은 요청 시 사용자의 브라우저에 대한 자격증명을 요청하므로 사용자가 사이트에 로그인하여 장기간 사용할 수 있는 사용자명과 비밀번호를 사용하지 않아도됩니다. 이는 보안을 강화하는 데 큰 도움이됩니다. 또한 기본 인증의 경우 인터넷 전송 중에 해당 정보가 암호화되어 전송되므로 더욱 안전합니다.

## 더 알아보기
- [Axios 공식 문서](https://github.com/axios/axios)
- [MDN Web Docs: HTTP 기본 인증](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)