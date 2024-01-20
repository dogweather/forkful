---
title:                "HTTP 요청 보내기"
html_title:           "Clojure: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

HTTP 요청을 보내는 것은 웹 서버와 데이터를 주고받는 방법입니다. 이를 통해 웹 서버가 데이터를 클라이언트에게 전달하고, 프로그래머들이 웹 서비스와 상호작용 할 수 있게 됩니다.

## 어떻게:

Ruby에서 HTTP 요청을 보내는 방법은 `Net::HTTP` 라이브러리를 사용합니다. 이를테면, HTTP GET 요청을 보내는 코드는 아래와 같습니다.

```Ruby
require 'net/http'

uri = URI('http://example.com')
res = Net::HTTP.get_response(uri)

puts res.body if res.is_a?(Net::HTTPSuccess)
```

결과는 http://example.com의 HTML 내용을 출력합니다.

## Deep Dive

- **역사적 맥락:** HTTP 요청은 웹의 초창기부터 있었습니다. 처음에는 단순히 정적 HTML 페이지를 가져오는 용도로 사용되었습니다.
- **대체 방법:** Ruby에서는 `net/http` 외에도 `httparty`나 `faraday` 같은 라이브러리를 통해 HTTP 요청을 보낼 수 있습니다. 각각은 사용 방법과 기능에서 차이가 있으니 상황에 맞게 선택하면 됩니다.
- **구현 세부사항:**  `Net::HTTP` 는 Ruby의 표준 라이브러리 중 하나입니다. 이 라이브러리를 사용하면 HTTP 프로토콜을 통해 데이터를 요청하고 받을 수 있습니다.

## 추가 자료

- Ruby 공식 문서 `Net::HTTP` [문서](https://ruby-doc.org/stdlib-3.0.1/libdoc/net/http/rdoc/Net/HTTP.html)
- `httparty` [Github 저장소](https://github.com/jnunemaker/httparty)
- `faraday` [Github 저장소](https://github.com/lostisland/faraday)