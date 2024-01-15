---
title:                "웹 페이지 다운로드하기"
html_title:           "Ruby: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜?

웹 페이지를 다운로드하려는 이유는 여러 가지가 있을 수 있지만, 대부분은 그 페이지에 있는 정보를 가져오거나 액세스하기 위해서일 것입니다.

## 어떻게?

간단한 Ruby 코드를 사용하여 웹 페이지를 다운로드하는 방법을 살펴보겠습니다. 먼저, `open-uri` 라이브러리를 사용하여 다운로드할 페이지의 URL을 불러옵니다. 그런 다음, `open` 메소드를 사용하여 웹 페이지의 내용을 다운로드합니다. 아래의 예시 코드를 참고해주세요.

```
require 'open-uri'

url = "https://www.example.com"
page = open(url).read

puts page
```

위의 코드를 실행하면, 해당 웹 페이지의 HTML 내용이 출력될 것입니다. 또한, `Net::HTTP` 라이브러리를 사용하여 HTTP 요청을 보내는 것으로도 웹 페이지를 다운로드할 수 있습니다. 하지만 이 방법은 좀 더 복잡하고 상세한 설정이 필요합니다. 자세한 내용은 아래의 "깊이 들어가기" 섹션을 참고해주세요.

## 깊이 들어가기

`open-uri`와 `Net::HTTP` 외에도, Ruby에서는 다양한 라이브러리를 사용하여 웹 페이지를 다운로드할 수 있습니다. 예를 들어, `HTTParty`라이브러리는 다양한 HTTP 요청을 보내는데 유용하며, `Nokogiri` 라이브러리를 사용하여 웹 페이지의 HTML을 파싱할 수 있습니다.

또한, HTTP 요청을 보낼 때 다양한 옵션을 설정할 수도 있습니다. 예를 들어, `open-uri`를 사용할 때는 `User-Agent` 헤더과 같은 다양한 헤더를 설정할 수 있으며, `Net::HTTP`를 사용할 때는 `set_request` 메소드를 통해 다양한 설정을 할 수 있습니다.

웹 페이지를 다운로드할 때 반드시 필요한 것은 아니지만, HTTP 프로토콜의 동작 방식과 웹 서버의 동작 방식에 대한 이해하고 있으면 더욱 유용하게 웹 페이지를 다운로드할 수 있을 것입니다.

## 더 보기

- [Ruby의 open-uri 라이브러리 공식 가이드 문서](https://ruby-doc.org//stdlib-2.6.5/libdoc/open-uri/rdoc/index.html)
- [Ruby의 Net::HTTP 라이브러리 공식 가이드 문서](https://ruby-doc.org//stdlib-2.6.5/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTParty 라이브러리 공식 문서](https://github.com/jnunemaker/httparty)
- [Nokogiri 라이브러리 공식 문서](https://nokogiri.org/tutorials/parsing_an_html_xml_document.html)