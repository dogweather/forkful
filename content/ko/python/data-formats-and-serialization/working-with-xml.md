---
date: 2024-01-26 04:35:14.218896-07:00
description: "\uBC29\uBC95: Python\uC758 `xml.etree.ElementTree` \uBAA8\uB4C8\uC740\
  \ XML \uC791\uC5C5\uC744 \uC704\uD55C \uB3C4\uAD6C\uB97C \uC81C\uACF5\uD569\uB2C8\
  \uB2E4. XML \uBB38\uC11C\uB97C \uD30C\uC2F1\uD558\uAE30."
lastmod: '2024-03-13T22:44:54.634962-06:00'
model: gpt-4-0125-preview
summary: "Python\uC758 `xml.etree.ElementTree` \uBAA8\uB4C8\uC740 XML \uC791\uC5C5\
  \uC744 \uC704\uD55C \uB3C4\uAD6C\uB97C \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

## 방법:
Python의 `xml.etree.ElementTree` 모듈은 XML 작업을 위한 도구를 제공합니다.

XML 문서를 파싱하기:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'제목: {title}, 저자: {author}')
```
샘플 출력:
```
제목: Learning Python, 저자: Mark Lutz
제목: Programming Python, 저자: Mark Lutz
```

XML 문서 생성하기:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## 심화 학습:
XML은 90년대 후반 SGML의 간소화된 하위 집합으로 생성되어 온라인 데이터 공유를 쉽게 하기 위한 것입니다. 웹 데이터에 대한 JSON의 인기가 높아지고 있음에도 불구하고, XML은 여전히 많은 기업, 설정 및 웹 서비스(SOAP, RSS)에서 중요한 역할을 합니다.

`xml.etree.ElementTree`의 대안으로는 `lxml`과 `minidom`이 있습니다. `lxml`은 더 빠르고 기능이 풍부하지만, `minidom`은 더 "DOM-스타일"의 XML 인터페이스를 제공합니다. 선택할 때 사용의 용이성, 성능, 그리고 특정 기능 요구사항을 고려하세요.

내부적으로, `ElementTree`는 요소 트리 모델을 사용합니다. 여기서 XML 파일의 각 구성 요소는 트리의 노드입니다. 이를 통해 직관적인 경로 표현과 검색이 가능하여 XML 데이터의 구조를 탐색하고 조작하기가 더 쉽습니다.

## 참고:
- Python `xml.etree.ElementTree` 모듈: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML 튜토리얼: https://www.w3schools.com/xml/
- XML 사양: https://www.w3.org/XML/
