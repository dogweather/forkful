---
title:                "HTML 파싱"
date:                  2024-01-20T15:33:54.376022-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇을 위해? 왜 사용하나요?)
HTML 파싱은 웹 페이지의 구조를 분석하는 과정입니다. 이를 통해 프로그래머는 웹 데이터를 추출하고, 조작하며, 웹의 정보를 자동으로 처리할 수 있습니다.

## How to: (어떻게 하나요?)
Ruby에서 HTML을 파싱하기 위해 `Nokogiri`라는 보석(라이브러리)을 자주 사용합니다. 간단한 예제를 살펴보죠.

```Ruby
require 'nokogiri'
require 'open-uri'

# 웹페이지를 불러옵니다.
html = URI.open("https://example.com")

# Nokogiri를 사용해 HTML을 파싱합니다.
doc = Nokogiri::HTML(html)

# CSS 선택자를 이용해 특정 요소를 찾습니다.
titles = doc.css('h1').map(&:text)

# 제목을 출력합니다.
puts titles
```

이 코드는 웹페이지의 `<h1>` 태그에 있는 모든 텍스트를 출력할 것입니다.

## Deep Dive (깊이 들여다보기)
HTML 파싱은 웹의 초창기부터 중요한 작업이었습니다. `Nokogiri`는 Ruby에서 가장 인기 있는 파싱 라이브러리 중 하나로, 그 편리함과 빠른 처리 능력 때문에 많이 사용됩니다. `Hpricot`과 같은 다른 라이브러리도 있지만, `Nokogiri`가 가장 널리 쓰입니다. `Nokogiri`는 내부적으로 `libxml2`을 사용해서 구문 분석을 하고, 문서 구조에 빠르고 쉽게 접근할 수 있게 해 줍니다.

HTML 파싱의 복잡성은 주로 HTML 자체의 비정형적인 특성 때문에 발생합니다. 올바르지 않은 HTML 마크업조차 처리할 수 있어야 하고, 다양한 형태의 HTML 문서에 대응할 수 있는 유연함이 필요합니다.

## See Also (더 보기)
- Nokogiri 공식 사이트: [http://www.nokogiri.org](http://www.nokogiri.org)
- Ruby-Doc for Nokogiri: [https://rubydoc.info/gems/nokogiri](https://rubydoc.info/gems/nokogiri)
- W3C HTML5 파싱 가이드라인: [https://www.w3.org/TR/html5/syntax.html#parsing](https://www.w3.org/TR/html5/syntax.html#parsing)
