---
title:                "Ruby: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 기본 인증과 함께 보내는 것이 왜 유용한지에 대해 알아보겠습니다. 일반적으로 웹 애플리케이션을 개발할 때 서버로부터 데이터를 받아오기 위해서는 HTTP 요청을 보내야 합니다. 그리고 해당 데이터를 받기 위해서는 인증이 필요할 수 있습니다. 따라서 기본 인증을 사용하면 보안적 측면에서 서버로부터 데이터를 안전하게 받을 수 있습니다.

## 어떻게

기본 인증을 사용하여 HTTP 요청을 보내는 방법을 간단한 코드 예제와 함께 살펴보겠습니다.

```ruby
require 'net/http'

uri = URI('http://example.com/data') # 데이터를 받을 서버의 URL을 설정합니다.
http = Net::HTTP.new(uri.host, uri.port) # Net::HTTP 라이브러리를 사용하여 HTTP 요청을 보낼 준비를 합니다.

# 기본 인증에 필요한 사용자 이름과 비밀번호를 설정합니다.
username = 'username'
password = 'password'

# 이후에 보낼 요청에 인증 정보를 포함시킵니다.
http.request(Net::HTTP::Get.new(uri.request_uri, initheader = { 'Authorization' => "Basic " + Base64.encode64(username + ":" + password).chomp })) do |response|
  # 서버로부터 받은 데이터를 출력합니다.
  puts response.body
end
```

위 예제에서는 `Net::HTTP` 라이브러리를 사용하여 HTTP 요청을 보내고, `Authorization` 헤더에 기본 인증 정보를 포함시킵니다. 그리고 `response` 객체를 통해 서버로부터 받은 데이터를 가져올 수 있습니다. 만약 인증이 실패하면 401 Unauthorized 응답 코드를 받게 됩니다.

### 예상 출력 결과:

```ruby
"This is an example response from the server."
```

## 깊게 파헤치기

이제 HTTP 요청을 보낼 때 기본 인증을 사용하는 방법을 좀 더 자세히 살펴보겠습니다. 기본 인증은 HTTP 요청 헤더에 `Authorization` 필드를 포함하여 사용자 이름과 비밀번호를 인코딩해 보냅니다. 이때 인코딩 방식은 Base64로, 인증 정보는 `username:password` 형태로 전송됩니다.

따라서 위 예제에서는 `Net::HTTP::Get.new` 함수를 사용하여 GET 요청을 보내고, `Authorization` 헤더에 기본 인증 정보를 포함시켰습니다. 이때 사용자 이름과 비밀번호는 `username:password` 형태로 이루어지며, 이를 Base64로 인코딩한 후 `Authorization` 헤더에 `"Basic "`을 추가하여 보냅니다. 따라서 서버는 해당 헤더의 값을 디코딩하여 인증을 확인하고, 알맞은 응답을 보낼 수 있습니다.

## 참고 자료

- [Net::HTTP RubyDoc](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP Basic Authentication (MDN Web Docs)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#HTTP_Basic_authentication)
- [Base64 RubyDoc](https://ruby-doc.org/stdlib-2.5.1/libdoc/base64/rdoc/Base64.html)