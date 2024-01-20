---
title:                "HTML 파싱"
html_title:           "Fish Shell: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

HTML 파싱은 HTML 텍스트를 분석하고 처리하는 과정입니다. 데이터 추출, 웹 크롤링, 웹 페이지의 내용 변경 등을 위해 프로그래머들이 이를 수행합니다.

## 어떻게 하는가:

ruby에서 HTML 파싱을 하기 위해 Nokogiri라는 젬을 사용합니다. 아래는 예제 코드입니다:

```Ruby
require 'nokogiri'
require 'open-uri'

doc = Nokogiri::HTML(open("http://www.example.com"))

doc.xpath('//h1').each do |node|
  puts node.text
end
```

이 코드는 "http://www.example.com" 웹페이지의 모든 h1 태그의 내용을 출력합니다.

## 깊게 알아보기:

HTML 파싱은 웹의 초기 단계부터 있어왔고, 데이터 마이닝, 검색 엔진 등에 중요한 역할을 합니다. ruby에서는 Nokogiri 외에도 Hpricot, HTML-Parser 등 다양한 HTML 파싱 라이브러리가 있습니다. Nokogiri는 libxml2 라이브러리를 기반으로 구현되어 있어 파워풀하고 유연합니다.

## 참고 자료:

1. Nokogiri documentation [here](http://nokogiri.org/)
2. HTML parsing with Hpricot [here](https://github.com/hpricot/hpricot)
4. Libxml2 library [here](http://xmlsoft.org/)