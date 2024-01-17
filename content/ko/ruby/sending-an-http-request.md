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

## 무엇인가? 왜그렇게 하는가?

HTTP 또는 HyperText Transfer Protocol은 인터넷에서 정보를 주고받는 데 사용되는 프로토콜입니다. HTTP 요청을 보내는 것은 당신이 웹 사이트를 방문하거나 정보를 가져오기 위해 인터넷을 사용할 때마다 발생합니다. 개발자들은 이를 프로그래밍해야 하는 이유는 HTTP 요청은 웹 애플리케이션의 핵심 기능 중 하나이기 때문입니다.

## 하는 방법:

**HTTP 요청 보내기**
```Ruby
require 'net/http'
require 'json'

url = URI("https://example.com/api/users")
response = Net::HTTP.get(url)
puts JSON.parse(response)
```
위의 예제 코드는 네트워크 요청을 만들고 응답을 가져와서 JSON으로 파싱하는 간단한 방법을 보여줍니다.

**HTTP POST 요청 보내기**
```Ruby
require 'net/http'
require 'json'

url = URI("https://example.com/api/users")
data = { username: "johnsmith", password: "password123" }
response = Net::HTTP.post(url, data.to_json)
puts response.body
```
위의 코드는 POST 요청을 만들고 데이터를 JSON으로 변환하여 전송하는 방법을 보여줍니다.

## 깊게 파고들기:

HTTP는 World Wide Web의 기본 통신 규약으로, 웹 브라우저와 웹 서버 간의 통신을 조정합니다. HTTP 요청의 간단한 형식은 클라이언트가 서버로 데이터를 보내는 데 사용됩니다. 프로그래머들은 HTTP를 사용하여 웹 사이트에서 정보를 가져오거나 웹 애플리케이션의 기능을 활성화하는 등 다양한 기능을 구현할 수 있습니다.

HTTP 요청을 보내는 방법에는 다양한 옵션이 있습니다. 위에서 예시로 든 코드는 Ruby의 `net/http` 라이브러리를 사용하여 간단하게 HTTP 요청을 보낸 것입니다. 하지만 프로그래머들은 `Faraday`나 `HTTParty`와 같은 다른 라이브러리를 사용하여 더 편리하고 간결하게 HTTP 요청을 보낼 수도 있습니다.

## 관련 자료:

- [Ruby `net/http` 라이브러리 문서](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [Faraday 라이브러리 문서](https://github.com/lostisland/faraday)
- [HTTParty 라이브러리 문서](https://github.com/jnunemaker/httparty)