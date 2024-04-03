---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:08.501360-07:00
description: "\uBC29\uBC95: Ruby\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uB824\uBA74\
  \ `gem install nokogiri`\uB85C 'Nokogiri' \uC82C\uC744 \uC124\uCE58\uD558\uC2ED\uC2DC\
  \uC624. Nokogiri\uB294 Ruby\uC5D0\uC11C HTML \uBC0F XML\uC744 \uB2E4\uB8E8\uB294\
  \ \uC2A4\uC704\uC2A4 \uAD70\uC6A9 \uB098\uC774\uD504\uC640 \uAC19\uC2B5\uB2C8\uB2E4\
  . \uB2E4\uC74C\uC740 \uAC04\uB2E8\uD55C \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.991672-06:00'
model: gpt-4-0125-preview
summary: "Ruby\uC5D0\uC11C HTML\uC744 \uD30C\uC2F1\uD558\uB824\uBA74 `gem install\
  \ nokogiri`\uB85C 'Nokogiri' \uC82C\uC744 \uC124\uCE58\uD558\uC2ED\uC2DC\uC624."
title: "HTML \uD30C\uC2F1"
weight: 43
---

## 방법:
Ruby에서 HTML을 파싱하려면 `gem install nokogiri`로 'Nokogiri' 젬을 설치하십시오. Nokogiri는 Ruby에서 HTML 및 XML을 다루는 스위스 군용 나이프와 같습니다. 다음은 간단한 예입니다:

```ruby
require 'nokogiri'
require 'open-uri'

# 웹사이트에서 HTML 내용을 불러옵니다
html_content = URI.open('http://example.com').read

# HTML을 파싱합니다
doc = Nokogiri::HTML(html_content)

# 제목을 추출합니다
title = doc.xpath('//title').text
puts "페이지의 제목은: #{title}"
```

이것은 대략 이런 결과를 내놓습니다: `페이지의 제목은: Example Domain`.

## 깊이 있게 살펴보기
초기 Ruby 시절에는 HTML을 파싱할 수 있는 옵션이 제한적이었습니다. REXML은 내장되어 있었지만 느렸습니다. 그다음 Hpricot이 등장했지만 서서히 사라졌습니다. Nokogiri는 2008년에 데뷔하여 Hpricot의 사용 편의성과 검증된 XML 툴킷인 libxml의 속도 및 파워를 결합했습니다.

파싱 세계에서는 항상 대안이 있습니다. 일부는 내장된 'rexml' 라이브러리나 또 다른 Ruby용 XML/HTML 파서인 'oga'를 선호합니다. 하지만 Nokogiri는 그 견고함과 속도, 그리고 방대한 기능 덕분에 여전히 많은 사람들의 선호도를 얻고 있습니다.

내부적으로 Nokogiri는 HTML을 문서 객체 모델(DOM)—트리 구조—로 변환합니다. 이를 통해 요소를 쉽게 탐색하고 조작할 수 있습니다. XPath와 CSS 선택자를 사용하면 필요한 정보의 어떤 부분이든 정확히 지정할 수 있습니다.

## 또한 보기
- Nokogiri 젬: [https://nokogiri.org/](https://nokogiri.org/)
- Ruby의 rexml 문서: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- 대안 파서 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- XPath에 대해 알아보기: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
