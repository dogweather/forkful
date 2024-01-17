---
title:                "웹 페이지 다운로드"
html_title:           "Ruby: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

웹 페이지 다운로드는 인터넷에서 웹 사이트의 콘텐츠를 컴퓨터에 저장하는 것을 말합니다. 이것은 프로그래머들이 필요한 데이터를 웹에서 수집하고 분석하기 위해 사용될 수 있습니다.

## 방법:

### 가장 간단한 방법:
```Ruby
require 'open-uri'
puts open("https://www.example.com").read
```
출력 결과:
```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="utf-8" />
  <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
</head>

<body>
<div>
  <h1>Example Domain</h1>
  <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
  <p><a href="https://www.iana.org/domains/example">More information...</a></p>
</div>
</body>
</html> 
```

### 다른 방법:
```Ruby
require 'net/http'
puts Net::HTTP.get(URI('https://www.example.com'))
```

## 깊이 파헤치기:

이 방법은 프로그래밍 언어와 서드파티 라이브러리에 따라 다를 수 있지만, 일반적으로 다음과 같은 기술을 사용하여 웹 페이지를 다운로드합니다:
 - HTTP 요청을 보내서 서버로부터 응답을 받습니다.
 - 응답 본문을 읽어서 컴퓨터에 저장합니다.
 - 응답 본문을 분석하여 필요한 정보를 추출합니다.
 
대부분의 프로그래밍 언어에는 HTTP 요청을 보내고 응답을 받는 데 도움이 되는 내장 또는 서드파티 라이브러리가 있습니다. 또한 웹 크롤링에 대해 더 자세히 알고 싶다면, 문서화된 API를 참조하십시오.

## 관련 자료:

[Mechanize gem](https://github.com/sparklemotion/mechanize) - 웹 크롤링과 웹 폼 작성을 도와주는 Ruby 라이브러리입니다.

[Requests library](https://requests.readthedocs.io/en/master/) - Python 으로 작성된 간단한 HTTP 요청을 보내는 라이브러리입니다.