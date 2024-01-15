---
title:                "http 요청 보내기"
html_title:           "Ruby: http 요청 보내기"
simple_title:         "http 요청 보내기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것에 대해 궁금하지 않은 사람은 없을 것입니다. 이것은 인터넷에서 정보를 가져오고 서비스와 상호 작용하는 매우 중요한 방법입니다. Ruby를 사용하여 간단하게 HTTP 요청을 보내는 방법에 대해 알아보겠습니다.

## 하우 투 (How To)

```Ruby
require 'net/http'

# GET 요청 보내기
url = URI.parse('https://www.example.com')
req = Net::HTTP::Get.new(url.to_s)
res = Net::HTTP.start(url.host, url.port,
  :use_ssl => url.scheme == 'https') do |http|
    http.request(req)
end
puts res.body # 요청의 내용 출력

# POST 요청 보내기
url = URI.parse('https://www.example.com/login')
req = Net::HTTP::Post.new(url.to_s)
req.set_form_data({username: 'username', password: 'password'})
res = Net::HTTP.start(url.host, url.port,
  :use_ssl => url.scheme == 'https') do |http|
    http.request(req)
end
puts res.body # 요청의 내용 출력
```

```ruby
# GET 요청의 경우, res.body에는 요청 결과로 받은 HTML 문서가 저장됩니다.
# POST 요청의 경우, 서버에서 보낸 응답의 내용을 res.body에서 확인할 수 있습니다.
```

## 딥 다이브 (Deep Dive)

HTTP 요청은 네트워크를 통해 데이터를 전송하는 기본적인 방법 중 하나입니다. 다양한 메소드(GET, POST, PUT, DELETE 등)와 헤더(인증 정보, 쿠키 등)를 사용하여 요청을 구성할 수 있습니다. 또한 응답에는 상태 코드, 응답 헤더, 본문 내용 등 다양한 정보가 포함될 수 있습니다.

## See Also
- [Ruby Net::HTTP 사용법](https://www.rubydoc.info/stdlib/net/Net/HTTP)
- [HTTP 메소드](https://developer.mozilla.org/ko/docs/Web/HTTP/Methods)
- [HTTP 응답 코드](https://developer.mozilla.org/ko/docs/Web/HTTP/Status)