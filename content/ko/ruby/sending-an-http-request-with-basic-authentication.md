---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Bash: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜 & 왜냐하면?

기본 인증을 사용한 HTTP 요청을 보내는 것은 웹 서버로의 안전한 접근을 위한 표준 방법인 것입니다. 프로그래머들이 이것을 사용하는 이유는 웹서버에 무작위 접속을 막기 위해 사용자이름과 비밀번호를 이용하여 인증 과정을 거치는 것입니다.

## 어떻게 하는가:

다음은 Ruby에서 `Net::HTTP`를 이용하여 기본 인증과 함께 GET 요청을 보내는 방법을 나타내는 코드입니다.

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/index.html')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'pass'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}
puts res.body
```

이 코드를 실행하면, 주어진 사용자 이름과 비밀번호를 사용하여 주어진 주소로 HTTP GET 요청이 실행됩니다.

## 심층 연구

기본 인증을 이용한 HTTP 요청은 웹의 초기부터 존재하였습니다. 1996년에 처음으로 RFC 1945에 문서화된 이후로 흔히 사용되어 왔습니다. 그러나 보안 문제로 인해 이에 대한 대체책들이 제시되었습니다. OpenID, OAuth, SAML 등이 그 예시입니다.

다른 방법으로 Ruby에서 기본 인증과 함께 HTTP 요청을 보내는 방법은 `RestClient`나 `HTTParty` 같은 라이브러리를 이용하는 것입니다.

구현 관련 세부 정보로는 Net::HTTP 모듈이 Ruby의 표준 라이브러리로 포함되어 있어 추가적으로 설치해주지 않아도 되며, 풍부한 기능을 제공한다는 것을 알 수 있습니다.

## 참고 자료

Ruby `Net::HTTP` 공식 문서: [https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)

`RestClient` 라이브러리: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)

`HTTParty` 라이브러리: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)

기본 인증에 대한 RFC 문서: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)