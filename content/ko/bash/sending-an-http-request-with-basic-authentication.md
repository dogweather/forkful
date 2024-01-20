---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Bash: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Bash를 이용한 기본 인증을 통한 HTTP 요청 보내기

## 왜 필요하고 왜 사용하나요?
HTTP 요청은 서버-클라이언트 모델에서 클라이언트가 서버에게 정보를 요청하는 방법입니다. 기본 인증을 사용하면 이 요청을 안전하게 보낼 수 있어, 중요 데이터를 보호할 수 있습니다.

## 어떻게 하는 건가요?
아래는 Bash에서 기본 인증을 사용하여 HTTP 요청을 보내는 간단한 예시입니다.

```Bash
#!/bin/bash

user="username"
password="password"
url="http://mywebsite.com"

curl -u $user:$password $url
```

실행 결과는 다음과 같습니다.

```Bash
<html>
...
</html>
```

HTTP 요청이 성공하면 웹페이지의 HTML을 보여줍니다.

## 깊게 알아보기
기본 인증은 HTTP/1.0에 처음 소개되었고, 아직까지 많이 사용됩니다. 그러나 누구나 패킷을 가로채서 내용을 볼 수 있기 때문에 안전을 보장하지 못합니다. 그래서 HTTPS가 등장하였고, 최대한 HTTPS를 사용하는 것이 좋습니다.

또한 cURL 외에도 `wget`같은 다른 도구들을 이용해서도 HTTP 요청을 보낼 수 있습니다. 

기본 인증은 `Authorization` 헤더에 사용자 이름과 패스워드를 base64 인코딩하여 추가하는 방식으로 구현됩니다.

## 참고자료
1. [cURL 공식 문서](https://curl.haxx.se/)
2. [HTTP 인증 - MDN 웹 문서](https://developer.mozilla.org/ko/docs/Web/HTTP/Authentication)