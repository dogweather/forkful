---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
웹 페이지를 다운로드하는 것은 웹 서버에서 웹 페이지의 내용을 요청한 뒤, 그 응답을 사용자의 컴퓨터에 저장하는 작업을 의미합니다. 프로그래머들은 이를 통해 웹 페이지에서 정보를 추출하거나, 웹 스크레이핑, 웹 콘텐츠 검색 등을 수행합니다.

## 어떻게
Ruby에서 웹 페이지를 다운로드하는 방법은 'net/http'라는 모듈을 사용하는 것입니다. 아래는 그 예시 코드입니다:
```Ruby
require 'net/http'
require 'uri'

def download_webpage(url)
  uri = URI.parse(url)
  response = Net::HTTP.get_response(uri)

  return response.body
end

puts download_webpage('http://www.example.com')
```
이 코드는 www.example.com의 웹 페이지를 다운로드하고, 그 내용을 출력합니다.

## 깊은 이해
1. **역사적 맥락**: 웹 페이지 다운로드는 인터넷이 상용화되면서 시작되었습니다. 초기 웹 브라우저인 Mosaic 등에서도 이 기능을 볼 수 있었습니다.

2. **대체 방법**: Ruby 외에도 Python, JavaScript 등 다양한 언어에서 웹 페이지 다운로드를 지원합니다. 라이브러리나 프레임워크를 사용하여 더 다양한 기능을 구현할 수도 있습니다.

3. **구현 세부사항**: net/http 모듈은 외부 서버에 연결하기 위한 TCP 소켓을 연결하고 HTTP 요청을 보냅니다. 서버는 이 요청에 대한 HTTP 응답을 반환하고, 이것을 우리는 웹 페이지의 내용으로 받게 됩니다.

## 참고 자료
- Ruby net/http 공식 문서: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- Ruby 외의 다른 언어에서 웹 페이지를 다운로드하는 방법: [https://www.twilio.com/blog/2017/05/http-requests-in-python-3.html](https://www.twilio.com/blog/2017/05/http-requests-in-python-3.html)