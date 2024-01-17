---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Javascript: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# What & Why?

HTTP 요청에 기본 인증을 함께 보내는 것은 인터넷을 통해 정보를 안전하게 전송하는 방법 중 하나입니다. 프로그래머들은 이 방법을 사용하여 웹사이트나 앱에 로그인할 때 사용자의 개인 정보를 보호하고 보안을 유지할 수 있습니다. 

# How to:

```Javascript
// 이 함수는 username과 password를 인자로 받아 기본 인증을 사용하여 HTTP 요청을 보냅니다.

function sendHttpRequest(username, password) {
  let xhr = new XMLHttpRequest();
  let url = "https://example.com/api";

  // username과 password를 이용하여 인증 헤더를 생성합니다.
  let authString = `${username}:${password}`;
  let encodedAuth = btoa(authString);

  // 인증 헤더를 설정합니다.
  xhr.setRequestHeader("Authorization", "Basic " + encodedAuth);

  xhr.onreadystatechange = function() {
    // 요청이 성공적으로 완료되면 응답을 출력합니다.
    if (xhr.readyState === 4 && xhr.status === 200) {
      console.log(xhr.responseText);
    }
  };

  // 요청을 보냅니다.
  xhr.open("GET", url);
  xhr.send();
}

sendHttpRequest("username", "password");
```

# Deep Dive:

기본 인증은 HTTP/1.0에서 처음 소개되었습니다. 이 방식은 매우 간단하지만 보안 측면에서 안전하지 않다는 단점이 있었습니다. 따라서 더 나은 보안 수단을 제공하는 다른 인증 방법들도 나오게 되었습니다. 그러나 여전히 많은 웹사이트들이 기본 인증을 사용하고 있으며, 여전히 많은 경우에 충분한 수준의 보안을 제공할 수 있습니다.

기본 인증은 사용자 이름과 비밀번호를 인코딩하여 인증 헤더에 추가하여 요청을 보냅니다. 이러한 인증 방식은 여전히 업계 표준이 아닙니다. 서드파티 라이브러리를 사용하여 보다 강력한 인증 방식을 구현하는 것도 좋은 대안입니다.

# See Also:

- [HTTP 기본 인증: MDN 웹 문서](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [인증에 대한 HTTP 스펙 RFC2617](https://tools.ietf.org/html/rfc2617)
- [NPM을 이용한 인증 라이브러리](https://www.npmjs.com/package/authentication)