---
title:                "XML 다루기"
date:                  2024-01-26T04:36:05.594945-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/swift/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
Swift에서 XML을 다룬다는 것은 XML 데이터를 파싱하고 생성하는 것을 의미합니다. 프로그래머들은 특히 XML이 표준 포맷인 시스템과 통합할 때 데이터 교환을 위해 이 작업을 합니다.

## 방법:
Swift는 XML 데이터를 파싱하기 위해 `XMLParser`와 `XMLDocument`를 제공합니다. 간단한 XML 문자열을 파싱하는 코드 조각은 다음과 같습니다:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget the party on Friday!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // 당신의 XMLParserDelegate
    parser.parse()
}
```

또한, `XMLDocument`를 사용하여 XML을 생성할 수도 있습니다:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

샘플 출력:

```xml
<note>
  <to>Tove</to>
</note>
```

## 심층 탐구
XML 또는 Extensible Markup Language은 90년대 후반부터 있었습니다. 이는 장황하지만 인간이 읽을 수 있어 복잡한 데이터 구조에 적합합니다. Swift의 XML 파싱 기능은 Python의 ElementTree나 Java의 JAXB에서 찾을 수 있는 것처럼 강력하지는 않지만, 기본적인 필요에 대해서는 충분히 해결할 수 있습니다.

새로운 시스템에서는 JSON과 같은 대안이 그 가벼운 무게와 덜 복잡한 파싱기 때문에 종종 선호되지만, XML은 여전히 많은 엔터프라이즈 및 레거시 시스템에서 두드러집니다.

Swift에서 XML을 다룰 때, `XMLParser`는 스트림 기반 파서이므로 XML 문서를 순차적으로 읽습니다. 큰 XML 파일의 경우, 이는 메모리 효율적입니다. 그러나 만약 당신이 단순함을 찾고 있고 당신의 XML 데이터가 합리적으로 작다면, `XMLDocument`를 사용하는 것이 더 간단할 수 있습니다.

## 참조
- [Apple의 XML 파싱 가이드](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML 튜토리얼](https://www.w3schools.com/xml/)