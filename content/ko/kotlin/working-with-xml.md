---
title:                "XML 다루기"
date:                  2024-01-26T04:33:44.060438-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/kotlin/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
XML을 다루는 것은 데이터 저장 및 전송을 위한 마크업 언어인 XML 문서를 파싱하고, 생성하고, 조작하는 것을 포함합니다. 프로그래머들이 이를 수행하는 이유는 많은 시스템들이 여전히 XML 형식으로 데이터를 교환하고 있으며, 이는 레거시 지원 및 기존 기술과의 통합을 위해 필요합니다.

## 방법:
Kotlin에서는 내장된 `javax.xml.parsers`를 파싱에 사용할 수 있습니다:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
XML 문서를 생성하기 위해서는 `javax.xml.transform`을 사용할 수 있습니다:

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
문서를 String으로 변환하는 샘플 출력은 단순히 XML 콘텐츠를 문자열 형식으로 나타냅니다.

## 심층 탐구
XML은 90년대부터 웹 및 소프트웨어 개발의 핵심으로, 그 가독성과 구조화된 계층으로 인해 선호되어 왔습니다. JSON이 웹 서비스에 있어 그 간결함과 더 작은 메시지 크기로 인해 인기를 얻었음에도 불구하고, XML은 기업 환경, SOAP 기반 웹 서비스 및 구성(예: 안드로이드 레이아웃 파일)에서 여전히 널리 사용되고 있습니다.

Kotlin/Java의 내장 기능 외에도 Simple XML Serialization 및 Jackson XML 모듈과 같은 다양한 라이브러리 및 API가 XML 처리를 위해 존재합니다. 하지만 `javax.xml.parsers`와 `javax.xml.transform`은 외부 의존성을 추가하지 않고도 대부분의 요구 사항을 충족시킵니다.

Kotlin에서 XML을 다룰 때는 캐릭터 인코딩을 올바르게 처리하고 XML 엔티티를 관리하여 XML 인젝션 공격을 방지하는 것과 같은 핵심 구현 세부사항에 주의해야 합니다. 데이터 무결성을 보장하기 위해 XML을 파싱할 때는 네임스페이스 복잡성과 스키마 검증에도 유의해야 합니다.

## 참고 자료
- [Kotlin 문서](https://kotlinlang.org/docs/reference/)
- [Java DOM 문서](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML 모듈](https://github.com/FasterXML/jackson-dataformat-xml)
