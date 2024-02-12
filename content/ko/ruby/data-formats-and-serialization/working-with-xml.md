---
title:                "XML 다루기"
aliases:
- /ko/ruby/working-with-xml/
date:                  2024-01-26T04:35:17.366603-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
XML을 다루는 것은 코드를 사용하여 XML(eXtensible Markup Language) 문서를 파싱, 생성 및 조작하는 것을 의미합니다. 프로그래머들은 XML이 통용되는 많은 웹 서비스, 설정 파일 및 데이터 교환형식과 상호작용하기 위해 이 작업을 수행합니다.

## 방법:
레일즈에 포함된 REXML을 사용하여 XML 스니펫을 파싱해 봅시다:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "Name: #{element.attributes['name']}, Color: #{element.attributes['color']}"
end
```
출력:
```
Name: apple, Color: green
Name: banana, Color: yellow
```

XML 생성 또한 간단합니다:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML 출력:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## 심층 분석:
XML의 뿌리는 1990년대로 거슬러 올라가며 웹 문서를 위한 SGML의 간소화된 부분집합으로 시작되었습니다. XML은 장황하지만 매우 구조화되어 있으며, 그것이 지금까지 유지된 이유입니다. XML은 유일한 선택지는 아니며—JSON과 YAML이 그 간결함으로 인해 인기를 끌고 있지만—XML은 많은 기업 및 레거시 시스템에서 강력한 위치를 유지하고 있습니다.

루비는 XML을 다루기 위한 몇 가지 방법을 제공합니다. REXML은 뛰어들기 쉬운 올-루비 라이브러리입니다. Nokogiri는 더 빠른 C 라이브러리를 래핑하는 젬으로, 속도와 추가 기능을 제공합니다. 둘 중에서 선택하는 것이라면, 더 작은 작업에 대해 REXML로 시작하고 더 많은 성능이 필요하면 Nokogiri로 넘어가십시오.

내부적으로, XML 파싱은 문자열을 DOM 또는 SAX 모델로 변환하는 것에 관한 것입니다. DOM은 메모리에 트리를 생성하는 반면, SAX는 문서를 스트리밍하고 파싱하면서 이벤트를 발생시킵니다. REXML은 두 모델을 모두 제공하지만, Nokogiri와 같은 C 확장을 사용하는 것보다는 느릴 수 있습니다.

## 참고:
- Ruby REXML 문서: https://www.rubydoc.info/stdlib/rexml
- Nokogiri 젬: https://nokogiri.org/
- XML 사양: https://www.w3.org/XML/
- SAX 소개: https://www.saxproject.org/
- YAML vs. JSON vs. XML 비교: https://www.upwork.com/resources/json-vs-xml-vs-yaml
