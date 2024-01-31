---
title:                "XML 다루기"
date:                  2024-01-26T04:32:51.390348-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

XML을 다루는 것은 코드를 사용하여 XML 콘텐츠를 파싱, 조작, 생성하는 것을 의미합니다. 프로그래머가 이 작업을 하는 이유는 XML이 인간이 읽을 수 있고 기계가 파싱할 수 있는 특성으로 인해 구성 파일, 데이터 교환 및 웹 서비스에 널리 사용되기 때문입니다.

## 방법:

XML을 파싱하는 방법은 다음과 같습니다:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>이번 주말 나를 잊지 마세요!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// 출력: User
```

그리고 XML을 생성하는 방법:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// 출력: <note><to>User</to></note>
```

## 심층 탐구

XML은 eXtensible Markup Language의 약자로, 90년대 후반부터 사용된 데이터 형식입니다. 인간과 기계 모두가 읽을 수 있는 문서를 인코딩하기 위한 규칙의 집합을 정의합니다. 역사적으로 XML은 그 유연성과 구조화된 계층구조로 인해 선호되어 왔으며, SOAP과 같은 웹 서비스 및 수많은 구성 파일에 대한 선택이 되었습니다.

XML의 대안으로는 자바스크립트와 함께 사용하기 쉽고 무게가 가볍다는 이유로 인기를 얻은 JSON(JavaScript Object Notation)이 있습니다. YAML은 인간 친화적이며 구성을 위한 일반적인 선택으로 가치를 지닌 또 다른 대안입니다.

JavaScript에서 XML은 DOMParser와 XMLSerializer 인터페이스를 사용하여 구현됩니다. XML DOM(문서 객체 모델)은 HTML을 다루듯이 XML 문서를 탐색하고 편집할 수 있게 해줍니다. JSON의 부상에도 불구하고 많은 레거시 시스템과 특정 산업이 여전히 데이터 교환을 위해 XML에 의존하고 있기 때문에 XML을 이해하는 것은 중요합니다.

## 참조

- MDN 웹 문서 (XML 파싱): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM 자습서): https://www.w3schools.com/xml/dom_intro.asp
- "XML이란 무엇인가?": https://www.w3.org/XML/
