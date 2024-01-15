---
title:                "기본 인증으로 Http 요청 보내기"
html_title:           "Bash: 기본 인증으로 Http 요청 보내기"
simple_title:         "기본 인증으로 Http 요청 보내기"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 것은 웹 서비스를 사용하려는 데 필요한 인증 정보를 제공하는 가장 기본적인 방법입니다.

## 하는 법

```Bash
# 기본 인증을 사용하여 HTTP 요청 보내기
curl -u username:password http://www.example.com
```

```
# 예상한 결과
HTTP/1.1 200 OK
Content-Type: text/html

<html>
<body>
...
</body>
</html>
```

## 딥 다이브

HTTP 요청을 보낼 때 기본 인증을 사용하는 방법은 `curl`이나 `wget`과 같은 명령 줄 도구를 사용하거나 `requests`와 같은 다른 언어의 HTTP 라이브러리를 사용하는 것입니다. 기본 인증은 보안 수준이 낮지만, 간단하고 실용적인 방식으로 다른 서비스나 자격증명이 없는 웹 사이트에 접근할 수 있어 많은 사용자에게 유용합니다.

## 참고

- [curl 공식 문서](https://curl.haxx.se/docs/manpage.html)
- [HTTP Requests in Bash](https://www.baeldung.com/http-request-bash)