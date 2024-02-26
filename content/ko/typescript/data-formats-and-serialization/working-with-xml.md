---
date: 2024-01-26 04:36:28.666757-07:00
description: "XML\uC744 \uB2E4\uB8EC\uB2E4\uB294 \uAC83\uC740 \uD504\uB85C\uADF8\uB798\
  \uBC0D\uC744 \uC0AC\uC6A9\uD574 XML \uB370\uC774\uD130\uB97C \uD30C\uC2F1, \uC870\
  \uC791, \uBC0F \uC791\uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC11C\uB85C \uB2E4\uB978 \uC2DC\uC2A4\
  \uD15C \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658, \uAD6C\uC131 \uD30C\uC77C,\
  \ \uD639\uC740 XML\uC5D0 \uC758\uC874\uD558\uB294 SOAP\uACFC \uAC19\uC740 \uD45C\
  \uC900\uC744 \uB2E4\uB8F0 \uB54C XML\uC744 \uCC98\uB9AC\uD569\uB2C8\uB2E4."
lastmod: '2024-02-25T18:49:51.898074-07:00'
model: gpt-4-0125-preview
summary: "XML\uC744 \uB2E4\uB8EC\uB2E4\uB294 \uAC83\uC740 \uD504\uB85C\uADF8\uB798\
  \uBC0D\uC744 \uC0AC\uC6A9\uD574 XML \uB370\uC774\uD130\uB97C \uD30C\uC2F1, \uC870\
  \uC791, \uBC0F \uC791\uC131\uD558\uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC11C\uB85C \uB2E4\uB978 \uC2DC\uC2A4\
  \uD15C \uAC04\uC758 \uB370\uC774\uD130 \uAD50\uD658, \uAD6C\uC131 \uD30C\uC77C,\
  \ \uD639\uC740 XML\uC5D0 \uC758\uC874\uD558\uB294 SOAP\uACFC \uAC19\uC740 \uD45C\
  \uC900\uC744 \uB2E4\uB8F0 \uB54C XML\uC744 \uCC98\uB9AC\uD569\uB2C8\uB2E4."
title: "XML \uB2E4\uB8E8\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
XML을 다룬다는 것은 프로그래밍을 사용해 XML 데이터를 파싱, 조작, 및 작성하는 것을 의미합니다. 프로그래머들은 서로 다른 시스템 간의 데이터 교환, 구성 파일, 혹은 XML에 의존하는 SOAP과 같은 표준을 다룰 때 XML을 처리합니다.

## 방법:
```TypeScript
import { parseString } from 'xml2js';

// 샘플 XML
const xml = `<note>
                <to>User</to>
                <from>Author</from>
                <heading>Reminder</heading>
                <body>회의를 잊지 마세요!</body>
             </note>`;

// XML을 JSON으로 해석
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// 해석이 성공적이라고 가정했을 때, 출력은 다음과 같을 수 있습니다:
// { note:
//    { to: ['User'],
//      from: ['Author'],
//      heading: ['Reminder'],
//      body: ['회의를 잊지 마세요!'] } 
}
```

## 심층 탐구
XML, 또는 확장 가능 마크업 언어는 90년대 후반부터 존재해왔습니다. 그 자체 설명적인 성격과 인간이 읽을 수 있는 형식은 RSS 피드, 구성 관리, 그리고 Microsoft Office Open XML과 같은 오피스 문서 형식과 같은 다양한 애플리케이션에서 초기부터 인기를 끌었습니다. 하지만, JSON에 비해 장황하며 시대가 변하고 있습니다. JSON은 더 가볍고 자바스크립트와의 네이티브 호환성으로 인해 웹 기반 API에 있어 주목받고 있습니다.

그럼에도 불구하고, XML은 죽지 않았습니다. 대규모 엔터프라이즈 시스템에서 사용되며, JSON으로 전환하지 않은 문서 표준에서 사용됩니다. TypeScript용 `xml2js` 또는 Python의 `lxml`과 같은 도구는 프로그래밍에서 XML 조작에 대한 지속적인 필요성을 입증합니다.

TypeScript는 JSON을 위해 내장된 지원을 가지고 있는 것처럼 XML을 위한 내장된 지원을 가지고 있지 않습니다. 대신, 라이브러리를 사용합니다. `xml2js`가 한 예입니다. 이는 XML을 JSON으로 변환해, 자바스크립트 전문가들이 데이터를 더 쉽게 다룰 수 있게 합니다.

## 참고 자료
- [MDN Web Docs의 XML에 대하여](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [xml2js npm 패키지](https://www.npmjs.com/package/xml2js)
- [W3Schools XML 자습서](https://www.w3schools.com/xml/)
