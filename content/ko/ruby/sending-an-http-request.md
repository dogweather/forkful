---
title:                "Ruby: HTTP 요청 보내기"
simple_title:         "HTTP 요청 보내기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 왜

HTTP 요청을 보내는 것이 왜 중요한지 궁금하신가요? 간단히 말씀드리자면, 우리는 인터넷에서 정보를 주고받는 데에 사용하는 프로토콜인 HTTP를 통해 서버로부터 데이터를 받아오기 위해 요청을 보내야 합니다. 예를 들어, 여러분이 어떤 웹 사이트를 방문하면 그 사이트의 서버로부터 여러분이 요청한 정보를 받아오게 됩니다. HTTP 요청을 보내는 것은 우리가 인터넷을 사용하는 일상생활에서 가장 중요한 부분 중 하나입니다.

## 방법

우선 간단한 예제를 통해 HTTP 요청이 어떻게 이루어지는지 알아보겠습니다. 루비에서는 'rest-client' 라이브러리를 사용하여 간단하게 HTTP 요청을 보낼 수 있습니다. 아래의 코드를 실행해보세요.

```Ruby
require 'rest-client'
url = 'https://jsonplaceholder.typicode.com/posts/1'
response = RestClient.get(url)
puts response
```

이 코드를 실행하면 해당 URL로 GET 요청을 보내고 그에 해당하는 응답을 받아옵니다. 또한, 코드에서 보이는 것처럼 'rest-client' 라이브러리를 사용하면 URL에 파라미터를 추가하여 더 자세한 요청을 보낼 수도 있습니다.

## 깊이 파고들기

HTTP 요청을 보낼 때 사용되는 메소드에는 GET, POST, PUT, DELETE 등이 있습니다. 이들 메소드를 각각 언제 사용해야 하는지에 대해 깊이 파고들어 알아보겠습니다.

* GET: 서버로부터 데이터를 받아오기 위해 사용되는 메소드입니다. 따라서 페이지나 정보를 요청할 때 주로 사용됩니다.

* POST: 서버로 데이터를 보내기 위해 사용되는 메소드입니다. 새로운 정보를 생성하거나 처리를 요청할 때 주로 사용됩니다.

* PUT: 이미 존재하는 정보를 수정하기 위해 사용되는 메소드입니다. POST와는 달리 해당 요청을 여러 번 보내더라도 동일한 결과가 나오도록 설계되었습니다.

* DELETE: 정보를 삭제하기 위해 사용되는 메소드입니다. 해당 요청을 보내면 삭제된 정보의 상태를 받아옵니다.

HTTP 요청에 관한 더 많은 정보는 아래의 링크를 참고하세요.

See Also/관련 링크:

* [HTTP Request Methods](https://www.w3schools.com/tags/ref_httpmethods.asp)
* [REST Client gem documentation](https://github.com/rest-client/rest-client)
* [Ruby Net::HTTP library documentation](https://ruby-doc.org/stdlib-2.6.1/libdoc/net/http/rdoc/Net/HTTP.html)