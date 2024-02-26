---
date: 2024-01-20 18:02:30.598852-07:00
description: "HTTP \uAE30\uBCF8 \uC778\uC99D\uC73C\uB85C \uC694\uCCAD\uC744 \uBCF4\
  \uB0B4\uB294 \uAC83\uC740, \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\
  \uD638\uB97C \uC774\uC6A9\uD574 \uBCF4\uC548\uB41C \uC790\uC6D0\uC5D0 \uC811\uADFC\
  \uD558\uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uC774 \uBC29\uC2DD\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC11C\uBC84\uAC00 \uC694\
  \uAD6C\uD558\uB294 \uC778\uC99D\uC744 \uC81C\uACF5, \uB370\uC774\uD130\uC5D0 \uC548\
  \uC804\uD558\uAC8C \uC811\uADFC\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-02-25T18:49:52.975068-07:00'
model: gpt-4-1106-preview
summary: "HTTP \uAE30\uBCF8 \uC778\uC99D\uC73C\uB85C \uC694\uCCAD\uC744 \uBCF4\uB0B4\
  \uB294 \uAC83\uC740, \uC0AC\uC6A9\uC790 \uC774\uB984\uACFC \uBE44\uBC00\uBC88\uD638\
  \uB97C \uC774\uC6A9\uD574 \uBCF4\uC548\uB41C \uC790\uC6D0\uC5D0 \uC811\uADFC\uD558\
  \uB294 \uBC29\uBC95\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uC774 \uBC29\uC2DD\uC744 \uC0AC\uC6A9\uD558\uC5EC \uC11C\uBC84\uAC00 \uC694\uAD6C\
  \uD558\uB294 \uC778\uC99D\uC744 \uC81C\uACF5, \uB370\uC774\uD130\uC5D0 \uC548\uC804\
  \uD558\uAC8C \uC811\uADFC\uD569\uB2C8\uB2E4."
title: "\uAE30\uBCF8 \uC778\uC99D\uC744 \uC0AC\uC6A9\uD55C HTTP \uC694\uCCAD \uBCF4\
  \uB0B4\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며, 왜?)
HTTP 기본 인증으로 요청을 보내는 것은, 사용자 이름과 비밀번호를 이용해 보안된 자원에 접근하는 방법입니다. 프로그래머들은 이 방식을 사용하여 서버가 요구하는 인증을 제공, 데이터에 안전하게 접근합니다.

## How to: (방법)
Ruby에서 HTTP 요청을 기본 인증과 함께 보내려면 `Net::HTTP` 라이브러리를 사용합니다. 예를 들어보죠.

```Ruby
require 'net/http'
require 'uri'

uri = URI('https://example.com')
username = 'user'
password = 'pass'

Net::HTTP.start(uri.host, uri.port, use_ssl: uri.scheme == 'https') do |http|
  request = Net::HTTP::Get.new(uri)
  request.basic_auth(username, password)
  response = http.request(request)
  
  puts response.body
end
```

위 코드는 기본 인증을 통해 `https://example.com`에 HTTP GET 요청을 보냅니다. 서버의 응답은 콘솔에 출력됩니다.

## Deep Dive (심층 분석)
기본 인증은 HTTP 1.0부터 사용되는 인증 방식입니다. Base64로 인코딩된 사용자 이름과 비밀번호를 `Authorization` 헤더에 넣어 전송합니다. 보안이 강화된 현대에는 기본 인증 대신 OAuth, API 키, 토큰 기반 인증과 같은 더 안전한 방법을 선호합니다.

`Net::HTTP`는 Ruby의 표준 라이브러리 중 하나로, HTTP 클라이언트 기능을 제공합니다. 하지만, 성능과 사용의 편의성 측면에서 `HTTParty`나 `Faraday` 같은 서드파티 라이브러리를 이용하는 경우도 많습니다.

안전하지 않은 네트워크에서 기본 인증을 사용할 때는 주의가 필요합니다. 누군가 트래픽을 캡처한다면, 사용자 이름과 비밀번호가 노출될 수도 있습니다. HTTPS를 사용하면 이러한 위험을 줄일 수 있습니다.

## See Also (추가 정보)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [HTTParty GitHub repository](https://github.com/jnunemaker/httparty)
- [Faraday GitHub repository](https://github.com/lostisland/faraday)
