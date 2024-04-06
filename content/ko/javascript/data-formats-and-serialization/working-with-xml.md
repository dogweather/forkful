---
date: 2024-01-26 04:32:51.390348-07:00
description: "\uBC29\uBC95: XML\uC740 eXtensible Markup Language\uC758 \uC57D\uC790\
  \uB85C, 90\uB144\uB300 \uD6C4\uBC18\uBD80\uD130 \uC0AC\uC6A9\uB41C \uB370\uC774\uD130\
  \ \uD615\uC2DD\uC785\uB2C8\uB2E4. \uC778\uAC04\uACFC \uAE30\uACC4 \uBAA8\uB450\uAC00\
  \ \uC77D\uC744 \uC218 \uC788\uB294 \uBB38\uC11C\uB97C \uC778\uCF54\uB529\uD558\uAE30\
  \ \uC704\uD55C \uADDC\uCE59\uC758 \uC9D1\uD569\uC744 \uC815\uC758\uD569\uB2C8\uB2E4\
  . \uC5ED\uC0AC\uC801\uC73C\uB85C XML\uC740 \uADF8 \uC720\uC5F0\uC131\uACFC \uAD6C\
  \uC870\uD654\uB41C \uACC4\uCE35\uAD6C\uC870\uB85C \uC778\uD574 \uC120\uD638\uB418\
  \uC5B4\u2026"
lastmod: '2024-04-05T22:51:10.040617-06:00'
model: gpt-4-0125-preview
summary: "XML\uC740 eXtensible Markup Language\uC758 \uC57D\uC790\uB85C, 90\uB144\uB300\
  \ \uD6C4\uBC18\uBD80\uD130 \uC0AC\uC6A9\uB41C \uB370\uC774\uD130 \uD615\uC2DD\uC785\
  \uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
weight: 40
---

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
