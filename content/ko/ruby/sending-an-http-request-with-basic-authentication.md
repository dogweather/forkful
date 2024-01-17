---
title:                "HTTP 기반 인증을 사용하여 http 요청 보내기"
html_title:           "Ruby: HTTP 기반 인증을 사용하여 http 요청 보내기"
simple_title:         "HTTP 기반 인증을 사용하여 http 요청 보내기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
HTTP 요청을 기본 인증으로 보내는 것은 일반적으로 우리가 인터넷에서 하는 모든 작업 중 하나입니다. 이것은 기본적으로 서버에 사용자 이름과 비밀번호를 제공하여 인증을 받는 과정입니다. 프로그래머들은 이것을 하는 이유는 보안 및 접근 제어를 위해서입니다.

## 어떻게?
```Ruby
require 'net/http'
uri = URI('https://example.com')
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true
request = Net::HTTP::Get.new(uri.path)
request.basic_auth('username', 'password')
response = http.request(request)
```
위의 코드 예시에서는 Ruby의 Net::HTTP 모듈을 사용하여 기본 인증을 사용하여 HTTP 요청을 보내는 방법을 보여줍니다. 사용자 이름과 비밀번호를 입력하여 요청 객체를 만든 다음, HTTP 요청을 보내고 응답을 받습니다.

## 딥 다이브
- 기본 인증은 브라우저가 웹 사이트에 대한 인증을 요청할 때 가장 일반적으로 사용되는 인증 방법 중 하나입니다.
- 암호 확인을 요구하지 않는 기본 인증보다 더 안전한 인증 방법으로는 Digest 인증이 있습니다.
- 위의 예시에서는 Net::HTTP 모듈을 사용하여 HTTP 요청을 보내는 방법을 보여주었지만 Faraday, HTTParty 등 다른 HTTP 요청 라이브러리를 사용할 수도 있습니다.

## 참고 자료
- [Ruby Net::HTTP Documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [Basic Authentication vs. Digest Authentication](https://www.ssl.com/article/understanding-beyond-basic-authentication/)