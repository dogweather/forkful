---
title:                "XML을 활용한 작업하기"
aliases:
- /ko/google-apps-script/working-with-xml/
date:                  2024-02-01T22:06:59.317081-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML을 활용한 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/google-apps-script/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

Google Apps Script에서 XML을 작업하게 해주는 것은 프로그래머들이 XML 데이터를 파싱하고, 조작하며, 생성할 수 있게 해주므로, 웹 서비스와 구성에 필수적입니다. 프로그래머들은 이 접근법을 구식 시스템과의 통합, 웹 스크래핑 수행 또는 여전히 데이터 교환을 위해 JSON보다 XML에 의존하는 수많은 API와 통신하기 위해 채택합니다.

## 방법:

Google Apps Script는 XML 데이터 작업을 위한 `XmlService`를 제공합니다. 아래에서는 XML 문자열을 파싱하는 방법, 그 내용을 수정하는 방법, 그리고 새로운 XML 문자열을 생성하는 방법을 보여줍니다.

XML 문자열 파싱하기:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Logs: Hello
}
```

XML 수정하기 위해 새로운 자식 요소를 추가하고 싶을 수 있습니다:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // 새로 추가된 자식 요소를 포함한 새 XML 문자열을 로깅합니다
}
```

처음부터 XML 문자열 생성하기:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // 출력값: <root><child>Hello World</child></root>
}
```

## 심층 탐구

역사적으로, XML(Extensible Markup Language)은 JSON이 가벼운 대안으로 등장하기 전에 데이터 교환을 위한 사실상의 표준이었습니다. XML의 자세한 문법과 엄격한 파싱 모델은 견고하지만 다소 무거운 데이터 형식을 제공했습니다. Google Apps Script에서 `XmlService` API는 XML 데이터의 생성, 파싱, 그리고 조작을 캡슐화하여, 다양한 구식 및 기업 시스템, SOAP 웹 서비스, 그리고 애플리케이션의 구성 파일에서의 지속적 중요성을 인정합니다.

JSON이 현대 웹 개발에서 그 단순성과 JavaScript와의 원활한 사용으로 인해 널리 사용되고 있음에도 불구하고, 문서 검증 및 구조화된 계층이 중요한 영역에서는 XML이 여전히 관련성을 유지하고 있습니다. 그러나 새 프로젝트에 있어서, 특히 웹 API 쪽으로 기울고 있는 경우, 가벼운 특성과 JavaScript와의 끊김없는 통합으로 인해 JSON이 종종 더 실용적인 선택입니다.

구식 시스템 또는 특정 기업 API와의 통합이 필요한 환경에서 작업하는 개발자들에게 Google Apps Script에서 XML과 그 처리를 이해하는 것이 중요합니다. 그러나, 새 프로젝트를 시작하거나 유연성이 중요할 때는 JSON과 같은 대안을 고려해 XML의 필요성을 평가하는 것이 좋습니다.
