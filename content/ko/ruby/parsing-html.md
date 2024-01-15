---
title:                "HTML 구문 분석"
html_title:           "Ruby: HTML 구문 분석"
simple_title:         "HTML 구문 분석"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## 왜?

웹 페이지는 복잡한 구조와 다양한 형식으로 구성될 수 있습니다. 이러한 구조를 이해하고 원하는 데이터를 추출하기 위해서는 HTML 파싱이 필요합니다.

## 어떻게?

```ruby
require 'nokogiri'
require 'open-uri'

# 웹 페이지에서 HTML 코드를 가져옵니다.
html = open('https://www.example.com').read

# Nokogiri를 사용하여 가져온 HTML 코드를 파싱합니다.
doc = Nokogiri::HTML(html)

# CSS 선택자를 사용하여 원하는 데이터를 추출합니다.
title = doc.css('h1').text
paragraphs = doc.css('p').map(&:text)

# 추출한 데이터를 출력합니다.
puts title
puts paragraphs
```
### 결과:
```
Example Domain
["This domain is for use in illustrative examples in documents. You may use this domain in literature without prior coordination or asking for permission.", "More information..."]
```

## 깊게 파고들기

HTML은 계층 구조로 이루어져 있습니다. 따라서 CSS 선택자를 사용하여 특정 요소를 선택하면 해당 요소의 부모, 자식, 형제 요소들도 함께 선택할 수 있습니다. 또한 CSS 선택자 외에도 XPath를 사용하여 더 복잡한 선택 기능을 사용할 수 있습니다. 

또한 HTML 파싱은 다양한 라이브러리를 사용할 수 있습니다. Nokogiri 외에도 Mechanize, Hpricot, Beautiful Soup 등 다양한 옵션을 사용할 수 있습니다.

## 참고하기

- [Nokogiri GitHub 페이지](https://github.com/sparklemotion/nokogiri)
- [CSS 선택자로 웹스크래핑하기](https://www.bytheway.io/2012/04/25/scraping-web-pages-with-css-selectors/)
- [Nokogiri와 XPath](http://ruby.bastardsbook.com/chapters/html-parsing/?dir=2)