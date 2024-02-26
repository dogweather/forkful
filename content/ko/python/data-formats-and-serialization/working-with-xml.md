---
date: 2024-01-26 04:35:14.218896-07:00
description: "\"XML \uC791\uC5C5\"\uC740 \uD504\uB85C\uADF8\uB798\uBC0D\uC744 \uC0AC\
  \uC6A9\uD558\uC5EC XML(eXtensible Markup Language) \uD30C\uC77C\uC744 \uC77D\uACE0\
  , \uC0DD\uC131\uD558\uBA70 \uC218\uC815\uD558\uB294 \uACFC\uC815\uC744 \uC758\uBBF8\
  \uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774 \uC791\uC5C5\
  \uC744 \uD558\uB294 \uC774\uC720\uB294 XML\uC774 \uD50C\uB7AB\uD3FC \uB3C5\uB9BD\
  \uC801\uC778 \uD2B9\uC131\uACFC \uC790\uCCB4 \uAE30\uC220 \uD615\uC2DD \uB54C\uBB38\
  \uC5D0 \uB370\uC774\uD130 \uAD50\uD658\uC5D0 \uB110\uB9AC \uC0AC\uC6A9\uB418\uAE30\
  \u2026"
lastmod: '2024-02-25T18:49:51.670496-07:00'
model: gpt-4-0125-preview
summary: "\"XML \uC791\uC5C5\"\uC740 \uD504\uB85C\uADF8\uB798\uBC0D\uC744 \uC0AC\uC6A9\
  \uD558\uC5EC XML(eXtensible Markup Language) \uD30C\uC77C\uC744 \uC77D\uACE0, \uC0DD\
  \uC131\uD558\uBA70 \uC218\uC815\uD558\uB294 \uACFC\uC815\uC744 \uC758\uBBF8\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774 \uC791\uC5C5\uC744\
  \ \uD558\uB294 \uC774\uC720\uB294 XML\uC774 \uD50C\uB7AB\uD3FC \uB3C5\uB9BD\uC801\
  \uC778 \uD2B9\uC131\uACFC \uC790\uCCB4 \uAE30\uC220 \uD615\uC2DD \uB54C\uBB38\uC5D0\
  \ \uB370\uC774\uD130 \uAD50\uD658\uC5D0 \uB110\uB9AC \uC0AC\uC6A9\uB418\uAE30\u2026"
title: "XML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
"XML 작업"은 프로그래밍을 사용하여 XML(eXtensible Markup Language) 파일을 읽고, 생성하며 수정하는 과정을 의미합니다. 프로그래머들이 이 작업을 하는 이유는 XML이 플랫폼 독립적인 특성과 자체 기술 형식 때문에 데이터 교환에 널리 사용되기 때문입니다.

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
