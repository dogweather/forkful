---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Ruby: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 이유

HTTP 요청을 기본 인증과 함께 보내는 이유는 인터넷 서비스를 사용하기 위해서입니다. 많은 웹사이트들이 사용자를 인증하기 위해 기본 인증을 사용합니다. 그래서, HTTP 요청을 보낼 때 기본 인증을 포함하려면, 기본 인증 정보를 요청과 함께 보내야 합니다.

## 어떻게

기본 인증을 포함한 HTTP 요청을 보내는 방법에 대해 알아보겠습니다. 먼저, "net/http" 라이브러리를 로드해야 합니다. 그리고 인증된 사용자의 정보를 변수에 저장합니다.

```Ruby
require 'net/http'

# 기본 인증 정보
username = "johnsmith"
password = "password"
```

이제, 요청을 보내기 위한 URL을 변수에 저장합니다.

```Ruby
# 요청할 URL
url = URI("https://example.com/api/users")
```

HTTP 요청을 생성하고, 인증 정보를 추가합니다.

```Ruby
# HTTP 요청 생성
request = Net::HTTP::Get.new(url)

# 인증 정보 추가
request.basic_auth(username, password)
```

마지막으로, 요청을 보내고 응답을 변수에 저장합니다.

```Ruby
# 요청 보내기
response = Net::HTTP.start(url.hostname, url.port, use_ssl: true) do |http|
  http.request(request)
end
```

응답을 출력해보겠습니다.

```Ruby
# 응답 출력
puts response.body

# 결과 예시
# {"id": 12345, "username": "johnsmith", "email": "johnsmith@example.com"}
```

위 예제는 기본적인 GET 요청을 보내는 방법이며, 다른 HTTP 메소드나 다른 인증 방식을 사용하려면 코드를 약간 수정하면 됩니다.

## 딥 다이브

기본 인증은 HTTP 요청에 대한 보안 수준이 낮고 보안이 취약하다는 단점이 있지만, 간단하게 인증을 필요로 하는 서비스나 애플리케이션에 적합합니다. 따라서, 중요한 정보나 민감한 데이터를 주고받아야 할 경우에는 다른 인증 방식을 사용하는 것이 좋습니다.

## 관련 링크

- [Ruby HTTP 요청 공식 문서 (영어)](https://ruby-doc.org/stdlib-2.6.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP Basic Authentication 설명서 (영어)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)