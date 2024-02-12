---
title:                "XML 다루기"
date:                  2024-01-26T04:29:12.992706-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML 다루기"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/working-with-xml.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
XML은 문서를 인코딩하기 위한 마크업 언어로, 사람과 기계 모두가 읽을 수 있는 방식입니다. 웹 서비스, 구성 파일, 데이터 교환에서 중요한 역할을 하며, 데이터를 구조화된 계층 형식으로 전달합니다.

## 사용법:
Clojure는 XML 파싱과 생성을 위해 `clojure.data.xml` 라이브러리를 제공합니다. 먼저 XML을 파싱해 보겠습니다:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; XML 문자열 파싱
  (println parsed))
```
출력:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Clojure 구조체에서 XML을 생성하기 위해서는:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
출력:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## 심층 분석
XML은 90년대 후반 SGML의 단순화된 하위 집합으로 시작했으며, 웹 데이터를 위해 생겼습니다. SOAP과 XHTML과 같은 기술의 사용이 폭발적으로 증가하며 사용량이 급증했지만, 가벼움과 단순함을 선호하는 JSON으로부터 경쟁을 받게 되었습니다.

Clojure에서의 XML 접근은 기능적이고 데이터 중심적이며, 언어의 정신을 유지합니다. `clojure.data.xml`은 단지 하나의 옵션이며, 기본적인 요구 사항에는 `clojure.xml`을 사용할 수 있고, Java 상호 운용성에는 JAXB나 DOM4J와 같은 강력한 도구를 사용할 수 있습니다.

매우 큰 XML 문서를 다룰 때의 성능과 메모리 오버헤드는 상당할 수 있습니다. StAX와 같은 스트리밍 파서가 도움이 될 수 있지만, 이를 위해서는 Java 영역으로 들어가야 합니다.

## 참고 자료
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
